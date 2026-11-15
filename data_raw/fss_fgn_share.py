"""Build data_raw/fgn_share.csv from FSS monthly press releases.

Source: FSS press release board, monthly "외국인 증권투자 동향" releases
        https://www.fss.or.kr/fss/bbs/B0000188/list.do?menuNo=200218
Output: data_raw/fgn_share.csv with columns
        pub_date  : release date on the board (YYYY-MM-DD)
        ref_month : month the figures refer to (YYYY-MM)
        fgn_share : foreign holdings / listed bond balance, % (month end)

Stages (each one caches its output, so reruns are cheap):
  list     -> data_raw/original/fss/index.csv      (title, nttId, release date)
  download -> data_raw/original/fss/<ref_month>.<pdf|hwp>
  text     -> data_raw/original/fss/<ref_month>.txt
  parse    -> data_raw/fgn_share.csv

Run from the "Final Project" folder:
  python3 data_raw/fss_fgn_share.py [list|download|text|parse|all]
HWP files need hwp5txt (pip package pyhwp). This script is only for building the
manual csv; the R file reads the csv.
"""

import csv
import html
import re
import subprocess
import sys
import tempfile
import time
import urllib.parse
import urllib.request
from pathlib import Path

BASE = "https://www.fss.or.kr"
LIST = BASE + "/fss/bbs/B0000188/list.do"
VIEW = BASE + "/fss/bbs/B0000188/view.do"
QUERIES = ["외국인 증권투자", "외국인 증권매매"]
# releases whose title has no year: ref_month -> nttId
EXTRA = {"2014-10": "11305"}  # "10월 외국인 주식 2.1조원 순매도, 채권 1.3조원 순투자"
OUT_DIR = Path("data_raw/original/fss")
INDEX = OUT_DIR / "index.csv"
CSV_OUT = Path("data_raw/fgn_share.csv")
UA = {"User-Agent": "Mozilla/5.0 (FE final project data collection)"}

# "2010. 3월 외국인 증권투자 동향", "2018.11월중 ...", "2026년 9월 ...", "`14.7월 ...",
# "2019년도 10월 ...", "2010. 12월 외국인 증권투자 및 보유 현황", "`14년 12월 외국인 증권매매 동향"
TITLE_RE = re.compile(r"^\s*['`]?(\d{2,4})\s*(?:년도|[.년])\s*(\d{1,2})\s*월\s*중?\s*"
                      r"외국인\s*증권\s*(?:투자|매매)\s*(?:동향|및\s*보유\s*현황)")


def get(url, params=None, referer=None):
    if params:
        url = url + "?" + urllib.parse.urlencode(params)
    h = dict(UA)
    if referer:
        h["Referer"] = referer
    for attempt in range(4):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=h), timeout=60) as r:
                return r.read()
        except Exception as e:  # noqa: BLE001
            if attempt == 3:
                raise
            time.sleep(2 + 3 * attempt)


def stage_list():
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    rows = {ref: (ntt, "(no year in title)") for ref, ntt in EXTRA.items()}
    for query in QUERIES:
        page = 1
        while True:
            body = get(LIST, {"menuNo": "200218", "searchCnd": "1", "searchWrd": query,
                              "pageIndex": page}).decode("utf-8", "replace")
            links = re.findall(r'href="/fss/bbs/B0000188/view\.do\?nttId=(\d+)[^"]*"[^>]*>\s*([^<]+?)\s*<', body)
            if not links:
                break
            for ntt, title in links:
                title = html.unescape(title).strip()
                m = TITLE_RE.match(title)
                if not m or "지역별" in title:
                    continue
                y, mo = int(m.group(1)), int(m.group(2))
                y = y + 2000 if y < 100 else y
                rows.setdefault(f"{y:04d}-{mo:02d}", (ntt, title))
            page += 1
            time.sleep(0.5)
    # release date from each view page (the list page does not show it reliably)
    out = []
    for ref, (ntt, title) in sorted(rows.items()):
        body = get(VIEW, {"nttId": ntt, "menuNo": "200218"}).decode("utf-8", "replace")
        d = re.search(r"등록일</dt>\s*<dd[^>]*>\s*(\d{4}-\d{2}-\d{2})", body) or \
            re.search(r"(\d{4}-\d{2}-\d{2})", body)
        files = re.findall(r'href="(/fss/cmmn/file/fileDown\.do\?[^"]+)"[^>]*>\s*([^<]*?)\s*<', body)
        names = re.findall(r'([^<>"/]+?\.(?:pdf|hwp|hwpx))\b', body)
        out.append({"ref_month": ref, "nttId": ntt, "title": title,
                    "pub_date": d.group(1) if d else "",
                    "files": " | ".join(sorted(set(html.unescape(f[0]) for f in files))),
                    "names": " | ".join(dict.fromkeys(n.strip() for n in names))})
        time.sleep(0.3)
    with INDEX.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(out[0]))
        w.writeheader()
        w.writerows(out)
    print(f"list: {len(out)} releases, {out[0]['ref_month']} .. {out[-1]['ref_month']}")


def read_index():
    with INDEX.open(encoding="utf-8") as f:
        return list(csv.DictReader(f))


MIN_TEXT = 3000  # a PDF with less text than this is a scanned image; use the HWP


def stage_download():
    """Keep the PDF and, when the PDF is missing or scanned, the HWP as well."""
    for r in read_index():
        m = r["ref_month"]
        pdf, hwp = OUT_DIR / f"{m}.pdf", OUT_DIR / f"{m}.hwp"
        pdf_ok = pdf.exists() and len(pdf_text(pdf)) >= MIN_TEXT
        if pdf_ok or hwp.exists():
            continue
        for link in r["files"].split(" | "):
            if not link:
                continue
            data = get(BASE + link, referer=f"{VIEW}?nttId={r['nttId']}&menuNo=200218")
            kind = "pdf" if data[:4] == b"%PDF" else "hwp" if data[:4] == b"\xd0\xcf\x11\xe0" else None
            out = {"pdf": pdf, "hwp": hwp}.get(kind)
            if out and not out.exists():
                out.write_bytes(data)
                print("download:", m, kind)
            time.sleep(0.3)
        if not pdf.exists() and not hwp.exists():
            print("download: no pdf/hwp for", m, r["title"])


def pdf_text(p):
    return subprocess.run(["pdftotext", "-raw", str(p), "-"], capture_output=True,
                          text=True, check=True).stdout


def hwp_text(p):
    """HWP -> text via hwp5html (hwp5txt drops tables, which hold the share)."""
    with tempfile.TemporaryDirectory() as d:
        res = subprocess.run(["hwp5html", "--output", d, str(p)], capture_output=True, text=True)
        if res.returncode != 0:
            raise RuntimeError(res.stderr[-300:])
        x = Path(d, "index.xhtml").read_text(encoding="utf-8")
    x = re.sub(r"<style.*?</style>", " ", x, flags=re.S)
    x = re.sub(r"</(p|tr|td|th)>", "\n", x)
    return html.unescape(re.sub(r"<[^>]+>", " ", x))


def stage_text():
    for r in read_index():
        m = r["ref_month"]
        t, pdf, hwp = OUT_DIR / f"{m}.txt", OUT_DIR / f"{m}.pdf", OUT_DIR / f"{m}.hwp"
        if t.exists() and len(t.read_text(encoding="utf-8").strip()) >= MIN_TEXT:
            continue
        txt = pdf_text(pdf) if pdf.exists() else ""
        if len(txt.strip()) < MIN_TEXT and hwp.exists():
            txt = hwp_text(hwp)
        if len(txt.strip()) < MIN_TEXT:
            print("text: too little text for", m, len(txt.strip()))
        t.write_text(txt, encoding="utf-8")


NUM = r"△?\s*-?[0-9][0-9,]*(?:\.[0-9]+)?"
# summary sentences, oldest wording first:
#   "전체 상장채권 잔액(1,060조원) 대비 5.83%" (2010), "... 대비 보유비중 은 6.8%" (2010),
#   "(상장잔액대비 7.0%)" (2011), "(전체 상장채권의 6.9%)" (2011-13), "상장잔액의 11.2%" (later)
#   "전체 상장 채권잔액 (1,108조원) 의 6.7%" (2010-11)
SENT_RE = re.compile(r"(?:상장\s*채권\s*잔액\s*(?:\([^)]*\))?\s*(?:대비(?:\s*보유\s*비중\s*은)?|의)"
                     r"|상장\s*잔액\s*대비|전체\s*상장\s*채권의|상장\s*잔액의)"
                     r"[\s(]*([0-9]{1,2}\.[0-9]{1,2})\s*%")


def table_share(s, mo):
    """Reference-month row of the table "외국인 채권 (순)투자 및 보유 추이" (from 2013-09):
    순매수, 만기상환 등, 순투자, 보유금액, [상장잔액], 비중. The share is the last number."""
    for h in re.finditer(r"외국인\s*채권\s*(?:순\s*)?투자\s*및\s*보유\s*추이", s):
        seg = s[h.end():h.end() + 900]
        if "비중" not in seg[:250]:
            continue
        # "3월 ..." / "’15.1월 ..." / pdftotext sometimes emits "월 2 ..."
        row_re = r"(?:(?<![0-9])%d\s*월|월\s*%d)\s+((?:%s\s+){3,5}\(?%s\)?)" % (mo, mo, NUM, NUM)
        # last matching row: December releases also list the previous December first
        found = None
        for row in re.finditer(row_re, seg):
            last = re.findall(NUM, row.group(1))[-1]
            if "." in last and 0 < float(last.replace(",", "")) < 40:
                found = float(last.replace(",", ""))
        if found is not None:
            return found
    return None


def parse_share(txt, ref_month):
    """Foreign share of the listed bond balance (%) at the end of ref_month.
    Returns (share, source) with source "table" or "sentence"; (None, None) if not found."""
    s = re.sub(r"\s+", " ", txt)
    v = table_share(s, int(ref_month[5:]))
    if v is not None:
        return v, "table"
    m = SENT_RE.search(s)  # first hit = bond summary of the reference month
    return (float(m.group(1)), "sentence") if m else (None, None)


def stage_parse():
    rows, miss = [], []
    for r in read_index():
        t = OUT_DIR / f"{r['ref_month']}.txt"
        v, src = parse_share(t.read_text(encoding="utf-8", errors="replace"), r["ref_month"]) \
            if t.exists() else (None, None)
        if v is None:
            miss.append(r["ref_month"])
            continue
        rows.append({"pub_date": r["pub_date"], "ref_month": r["ref_month"], "fgn_share": v,
                     "source": src})
    with CSV_OUT.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["pub_date", "ref_month", "fgn_share", "source"])
        w.writeheader()
        w.writerows(rows)
    print(f"parse: {len(rows)} months written to {CSV_OUT}; not parsed: {miss}")


if __name__ == "__main__":
    stage = sys.argv[1] if len(sys.argv) > 1 else "all"
    for name, fn in [("list", stage_list), ("download", stage_download),
                     ("text", stage_text), ("parse", stage_parse)]:
        if stage in (name, "all"):
            fn()
