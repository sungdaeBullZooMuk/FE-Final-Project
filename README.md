# 국고채 금리에서 커진 미국의 몫 (FE Final Project)

국고채 금리의 하루 움직임 가운데 밤사이 미국 국채 금리 때문에 움직인 부분(미국 몫)이 2010년대보다 2020년대에 커졌다.
그 이유를 ① 미국 충격 확대 ② 1bp당 전이 강화 ③ 한국 고유 변동 축소로 나누어 검정한다(설계 v8).
처음 질문이었던 "미국 충격이 어떤 날 더 넘어오나"(시장 조건 v6, 충격 성격 v6, 변동성 v7)는 보조 분석 ①②③으로 함께 싣는다.
금융계량경제학(2026 가을) 기말 프로젝트.

처음 보는 사람은 `reports/연구_쉬운설명.pdf`부터 읽으면 된다.

## 폴더

```
Final Project/
├── reports/       읽을 결과물
│   ├── 연구_쉬운설명.pdf     처음 보는 사람용 설명서 13쪽 (원본 code/explainer_template.html)
│   ├── 연구_프로세스_도식.pdf 연구 전체 흐름 도식 6쪽 (원본 code/process_map.html)
│   ├── 본문_초안.pdf         논문 전체 초안 18쪽 (제목·초록, 1–7절, 참고문헌, 부록)
│   ├── 본문_표_초안.pdf      논문 본문 그림·표 초안
│   └── archive/   지난 결과물 (분석결과_정리_v6.pdf: 처음 설계 v6 시점 상세 결과, 더 갱신하지 않음)
├── docs/          설계 문서
│   ├── 미국몫_분해설계.md               현재 설계 (v8, 핵심 분석, 결과 전 커밋 8579148)
│   ├── 조건별전이_4부_변동성설계.md     보조 ③ 변동성 설계 (v7)
│   ├── 조건별전이_연구설계_통합안.md    보조 ①② 설계 (v6)
│   ├── 조건별전이_설계근거_검토.md      v6 수치·방법별 근거 검토
│   ├── 조건별전이_보고서_구성안.md      보고서 본문 절·표·쪽수 구성 (9절: 주제 전환 기록)
│   ├── 조건별전이_데이터전처리.md       데이터 수집·정리 규칙
│   ├── 조건별전이_변수확정_선행연구.md  변수별 선행연구 근거
│   ├── 조건별전이_연구개요_확정.md      첫 설계와 바뀐 점 기록 (v6 이전)
│   ├── 조건별전이_수업연계_개선안.md    수업 내용 연계 방안 (11-20)
│   ├── 미국 금리 … .md (2개)            팀원 원안 (@kim, 11-23)
│   ├── archive/   지난 문서 (주제 적합성 검토, 설계 검토)
│   └── reference/ 수업 안내문, 팀원 원안 docx (git 제외)
├── paper/        논문 본문 절별 원고 (md, [[표 n]]·[[그림 n]] 자리 표시)
├── code/          R 코드 (아래 실행 순서)
├── data/          분석용 csv (Data_team.csv)
├── data_raw/      KRX·금감원 원자료와 수집 스크립트 (data_raw/README.md)
├── output/        분석 결과 (RData, 표 csv, 그림 svg·png, 보고서 HTML)
└── submission/    제출용 단일 R 파일
```

설계 문서는 v6 → v7 → v8 순서로 쌓였다. 각 설계는 결과를 보기 전에 커밋했고, 뒤 설계가 앞 설계의 가설·결과를 바꾸지 않는다.

## 실행 순서 ("Final Project" 폴더에서)

```bash
Rscript code/01_collect_data.R          # 데이터 수집 (ECOS API 키 필요, 보통은 건너뜀)
Rscript code/02_analysis.R              # 분석 전체 → output/ (핵심 v8 + 보조 ①②③)
Rscript code/04_explainer.R             # reports/연구_쉬운설명.pdf
Rscript code/05_paper_tables.R          # reports/본문_표_초안.pdf (논문 본문 그림·표)
Rscript code/06_paper_draft.R           # reports/본문_초안.pdf (paper/*.md에 표·그림을 넣어 조립)
TEAM=Team3 Rscript code/00_build_submission.R   # submission/Team3_rcode.R
```

`code/03_results_report.R`는 v6 시점 결과 정리(reports/archive/분석결과_정리_v6.pdf)를 만들던 코드로, 더 갱신하지 않는다.
도식 PDF는 `code/process_map.html`을 Chrome으로 인쇄해 만든다.

## 제출물 (12/16)
`Team#_report.pdf`, `Team#_rcode.R`(submission/), `Data_team#.csv`(data/), `Team#_result.RData`(output/). ZIP 금지.
