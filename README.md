# 조건별 금리 전이 (FE Final Project)

미국 국채 금리 충격이 다음 날 한국 국채 금리로 넘어오는 정도가 어떤 조건에서 달라지는지 분석한다.
금융계량경제학(2026 가을) 기말 프로젝트.

처음 보는 사람은 `reports/연구_쉬운설명.pdf`부터 읽으면 된다.

## 폴더

```
Final Project/
├── reports/       읽을 결과물
│   ├── 연구_프로세스_도식.pdf 연구 전체 흐름 도식 5쪽 (원본 code/process_map.html)
│   ├── 연구_쉬운설명.pdf     처음 보는 사람용 설명서
│   └── 분석결과_정리.pdf     표·그림 포함 상세 결과
├── docs/          설계 문서
│   ├── 조건별전이_연구설계_통합안.md    현재 설계 (v6, 다른 문서보다 우선)
│   ├── 조건별전이_설계근거_검토.md      수치·방법별 근거 검토 (v6에 반영)
│   ├── 조건별전이_보고서_구성안.md      보고서 본문 절·표·쪽수 구성
│   ├── 조건별전이_연구개요_확정.md      첫 설계와 바뀐 점 기록 (일부 절은 통합안으로 대체)
│   ├── 조건별전이_데이터전처리.md       데이터 수집·정리 규칙
│   ├── 조건별전이_변수확정_선행연구.md  변수별 선행연구 근거
│   ├── archive/   지난 문서 (주제 적합성 검토, 설계 검토)
│   └── reference/ 수업 안내문, 팀원 원안 docx (git 제외)
├── code/          R 코드 (아래 실행 순서)
├── data/          분석용 csv (Data_team.csv)
├── data_raw/      KRX·금감원 원자료와 수집 스크립트 (data_raw/README.md)
├── output/        분석 결과 (RData, 표 csv, 그림 png, 보고서 HTML)
└── submission/    제출용 단일 R 파일
```

## 실행 순서 ("Final Project" 폴더에서)

```bash
Rscript code/01_collect_data.R          # 데이터 수집 (ECOS API 키 필요, 보통은 건너뜀)
Rscript code/02_analysis.R              # 분석 전체 → output/
Rscript code/03_results_report.R        # reports/분석결과_정리.pdf
Rscript code/04_explainer.R             # reports/연구_쉬운설명.pdf
TEAM=Team3 Rscript code/00_build_submission.R   # submission/Team3_rcode.R
```

## 제출물 (12/16)
`Team#_report.pdf`, `Team#_rcode.R`(submission/), `Data_team#.csv`(data/), `Team#_result.RData`(output/). ZIP 금지.
