<!--
부록 (초안). 표 A1–A4는 code/05_paper_tables.R가 만든다. 잔차 진단, 표본외 비교 전체, 14개 Holm 표는 분량 때문에 본문에서 빼고 결과 파일(res$diagnostics, res$table5, res$part3$holm14)에 둔다.
-->

## 부록

### A. 추가 표

[[표 A1]]

[[표 A2]]

[[표 A3]]

[[표 A4]]

### B. 재현 방법

제출물은 R 파일(`Team#_rcode.R`), 자료(`Data_team#.csv`), 결과 파일(`Team#_result.RData`) 세 개다. 두 파일을 같은 폴더에 두고 `Rscript Team#_rcode.R`을 실행하면 약 17분 뒤(대부분 변동성 예측의 롤링 GARCH와 분해의 블록 부트스트랩) `output/`에 결과 파일과 모든 표·그림이 생긴다. 필요한 패키지는 sandwich, lmtest, zoo, urca, tseries, glmnet, strucchange, rugarch, ggplot2, vars다. 앞부분의 자료 수집 코드는 ECOS API 키와 KRX·금융감독원 수동 자료가 필요해 `if (FALSE)` 블록 안에 두었다. 표 3과 그림 3은 `res$part5`, 표 7은 `res$part4`, 나머지 표는 `res$paper`에 있다. 난수는 `set.seed(2026)`으로 고정했고, API 키가 없는 새 환경에서 csv만으로 실행해 같은 값이 나오는 것을 확인했다.
