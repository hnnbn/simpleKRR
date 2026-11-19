
# 1. 패키지 함수 불러오기
devtools::load_all()

# 2. 시뮬레이션 데이터 생성
set.seed(1)
n = 150
X = matrix(runif(n, -1, 1), ncol = 1)
ftrue = function(x) sin(2*pi*x) + 0.5*cos(4*pi*x)
y = ftrue(X[,1]) + rnorm(n, sd = 0.1)

# 3. KRR 적합
fit = krr(X, y, rho = 10, lambda = 0.0001)

# 4. 새로운 데이터 생성
n_new = 300
X_new = matrix(seq(-1, 1, len = n_new), ncol = 1)

# 5. KRR 예측
pred = predict(fit, X_new)

# 6. 결과 확인
head(fit$coefficients)
head(pred)
dim(pred)
class(fit)

# 7. 결과 시각화
plot(X, y, col = "gray", pch = 16,
     xlab = "X", ylab = "y",
     main = "KRR Fitting")

lines(X_new[,1], pred, col = "blue", lwd = 2)
curve(ftrue(x), add = TRUE, col = "red", lwd = 2)
