# 패키지 불러오기
library(tidyverse)
library(corrplot)
library(CCA)

# 데이터 불러오기 & 정리
DF<-read.csv('fitness.csv')
dim(DF)
head(DF)

# X변수, Y변수 분리
X<-dplyr::select(DF, c(weight, waist, pulse))
Y<-dplyr::select(DF, c(chins, situps, jumps))

# 변수명 재정의
X<-rename(X, 'x1_체중'=weight,'x2_허리둘레'=waist, 'x3_맥박수'=pulse )
Y<-rename(Y, 'y1_턱걸이'=chins, 'y2_윗몸일으키기'=situps, 'y3_줄넘기'=jumps )

# 상관행렬 (전체)
R<-cor(cbind(X,Y))
print(R, digit=3)
corrplot.mixed(R, upper = 'ellipse')

# 정준상관계수 출력
fitness_cc<-cc(X,Y)
fitness_cc$cor

# x집단의 원 정준계수
fitness_cc$xcoef

# y집단의 원 정준계수
fitness_cc$ycoef

# x집단의 표준 정준계수
fitness_cc$xcoef*sapply(X,sd)
# => 즉, X로 만든 첫번째 정준변수는 '0.78체중-1.58허리둘레+0.06맥박수'이다. 이 값이 높으면 체중은 높고 허리둘레는 작다.(체중 대비 허리둘레)
# => X로 만든 두번째 정준변수 : 1.88체중-1.18허리둘레+0.23맥박수

# y집단의 원 정준계수
fitness_cc$ycoef*sapply(Y,sd)
# => 즉, Y로 만든 첫번째 정준변수는 '0.35턱걸이+1.05윗몸일으키기-0.7줄넘기'이다. 이 값이 높으면 턱걸이, 윗몸일으키기는 잘하고 줄넘기는 못한다.(턱걸이, 윗몸일으키기 대비 줄넘기)
# => Y로 만든 두번째 정준변수 : 0.38턱걸이-0.12윗몸일으키기-1.06줄넘기

# 정준점수 출력
X_scores<-fitness_cc$scores$xscores
colnames(X_scores)<-paste0("CVX", 1:3)

Y_scores<-fitness_cc$scores$yscores
colnames(Y_scores)<-paste0("CVY", 1:3)

fitness<-cbind(X,Y,X_scores, Y_scores)
round(fitness,digit=3)

# 정준점수 plot
round(cor(fitness),3)
corrplot.mixed(cor(fitness), lower='ellipse', upper='number')

corrplot.mixed(cor(fitness[,7:12]), lower='ellipse', upper='number')
pairs(fitness[,7:12])

# 정준적재 출력
fitness_cc$scores$corr.X.xscores # X 정준적재
fitness_cc$scores$corr.Y.yscores # Y 정준적재

# 정준적재 plot
fitness_loading<-rbind(fitness_cc$scores$corr.X.xscores,
                       fitness_cc$scores$corr.Y.yscores)
plot(fitness_loading[,1:2], pch=1, col="blue", xlim=c(-2,2), ylim = c(-2,2))
abline(v=0, h=0, lty=2)
text(fitness_loading[,1:2],labels=rownames(fitness_loading), pos=4, col="black" )

# 교차적재 출력
fitness_cc$scores$corr.X.yscores # X 교차적재
fitness_cc$scores$corr.Y.xscores # Y 교차적재

# 근사도 (제곱정준상관의 비율)
ccor<-fitness_cc$cor
sq_ccor<-ccor^2
cum_sq_ccor<-cumsum(sq_ccor)
G<-cum_sq_ccor/sum(sq_ccor)
GOF<-cbind(ccor, sq_ccor, cum_sq_ccor, G)
GOF
#=> 첫번째 정준상관계수는 내 데이터의 93 % 를 설명한다. (분산설명량)
# => X1~X3, Y1~Y3 총 6개의 변수 (6차원)에서 2차원으로 줄여도 전체의 93% 설명 가능하다.
