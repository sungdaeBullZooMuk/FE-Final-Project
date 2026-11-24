<!--
부록 (초안). 표 A1–A5는 code/05_paper_tables.R가 만든다.
-->

## 부록

### A. 추가 표

[[표 A1]]

[[표 A2]]

[[표 A3]]

[[표 A4]]

[[표 A5]]

### B. 재현 방법

제출물은 R 파일 하나(`Team#_rcode.R`), 자료 파일 하나(`Data_team#.csv`), 결과 파일 하나(`Team#_result.RData`)다. 다음 순서로 재현한다.

1. R 파일과 csv 파일을 같은 폴더에 두고, 그 폴더에서 `Rscript Team#_rcode.R`을 실행한다. 일반 노트북에서 약 2분 걸린다.
2. 필요한 패키지는 sandwich, lmtest, zoo, urca, tseries, glmnet, strucchange, rugarch, ggplot2다.
3. 파일 앞부분의 자료 수집 코드는 `if (FALSE)` 블록 안에 있어서 실행되지 않는다. ECOS API 키와 KRX·금융감독원 수동 자료가 필요하며, 제출한 csv를 만든 코드다.
4. 실행하면 `output/` 폴더에 결과 파일(`Team#_result.RData`), 모든 표(csv), 그림(png)이 생긴다. 본문의 표 1–6은 결과 파일의 `res$paper`와 `output/tables/paper_table*.csv`에 그대로 들어 있다.

난수는 `set.seed(2026)`으로 고정했다. API 키가 없는 새 환경에서 csv만으로 실행해 본문의 표와 같은 값이 나오는 것을 확인했다.
