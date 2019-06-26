Ponizej zostal przedstawiony oraz opisany kod odszumowania danych Iris

#wczytujemy dane
dane <- iris
library(MASS)
#biblioteka odpowiadajaca za stworzenie wykresow
library(scatterplot3d)
dane <- unique(dane[,1:4])
dane
#liczymy distance Euclidean
d1 <- dist(dane)
#liczymy Kruskal's Non-Metric Multidimensional Scaling
isoMDS(d1, k=3) -> wsp
#rysujemy 
scatterplot3d(wsp$points, angle=70, color = colors)
#wpisujemy kolory
colors <- c("red", "yellow", "green")
#tutaj zamieniamy setosa i t.p. na nasze kolory
colors[iris[,5]] -> colors
colors[1:149] -> colors 
colors
#Obliczamy dekompozycje wartosci pojedynczej prostokatnej macierzy.
svd(dane) -> faktoryzacja
str(faktoryzacja)
U <- faktoryzacja$u
D <- faktoryzacja$d
V <- faktoryzacja$v
U
D
V
U %*% diag(D) %*% t(V)
D^2/sum(D*D)
#pakujemy dane z usunietymi "szumami"
paczka <- list(u=U[, 1:2], d=D[1:2], v=V[,1:2])
str(paczka)
149*4/(149*2+2+4*2)
U1 <- paczka$u
D1 <- paczka$d
VP1 <- paczka$v
#rozpakujemy nasze skompresowane dane oraz liczymy distanc pomiedzy nimi
U1 %*% diag(D1) %*% t(VP1) -> rozpakowane.dane
d2 <- dist(rozpakowane.dane)
wsp2 <- isoMDS(d2, k=3)
#rysujemy nowy wykres 
scatterplot3d(wsp2$points, angle=70, color = colors)

__________________________________________________________________________________

library(matlib)

# Meoda Elominacji Gaussa
#1 a.
A <- matrix(c(1, 3, 4,
              4, 2, -2,
              2, 1, 1), 3, 3, byrow = TRUE)

b <- c(0, 0, 8)

gaussianElimination(A, b)

#gaussianElimination(A, b, verbose=TRUE, fractions=TRUE)

#1 b.

A <- matrix(c(2, -4, 3, -4,
              -1, 3, -2, 1,
              2, -1, 1, 2,
              1, 2, -1, 1), 4, 4, byrow = TRUE)
b <- c(2, 4, 3, 1)

gaussianElimination(A, b)

#2 a.

A <- matrix(c(1, 2, 3,
              2, 3, -1,
              3, 1, 2), 3, 3, byrow = TRUE)
b <- c(1, 3, 2)
gaussianElimination(A, b)

#2 b.

A <- matrix(c(1, 2, 3, 4,
              2, 3, 4, 1,
              3, 4, 1, 2,
              4, 1, 2, 3), 4, 4, byrow = TRUE)
b <- c(11, 12, 13, 14)
gaussianElimination(A, b)

#3

A <- matrix(c(2, 1, 1, -1,
              2, 2, 0, -1,
              1, -1, 1, 1,
              0, 2, 1, 1), 4, 4, byrow = TRUE)
b <- c(8, 1, 4, 16)
gaussianElimination(A, b)

#----------------------------------------------------------------------

#Metoda macierzy odwrotnej

#1 a.
A <- matrix(c(1, 3, 4,
              4, 2, -2,
              2, 1, 1), 3, 3, byrow = TRUE)

b <- c(0, 0, 8)
solve(A) %*% b

#1 b.

A <- matrix(c(2, -4, 3, -4,
              -1, 3, -2, 1,
              2, -1, 1, 2,
              1, 2, -1, 1), 4, 4, byrow = TRUE)
b <- c(2, 4, 3, 1)
solve(A) %*% b

#2 a.
A <- matrix(c(1, 2, 3,
              2, 3, -1,
              3, 1, 2), 3, 3, byrow = TRUE)
b <- c(1, 3, 2)
solve(A) %*% b

#2 b.
A <- matrix(c(1, 2, 3, 4,
              2, 3, 4, 1,
              3, 4, 1, 2,
              4, 1, 2, 3), 4, 4, byrow = TRUE)
b <- c(11, 12, 13, 14)
solve(A) %*% b

#3
A <- matrix(c(2, 1, 1, -1,
              2, 2, 0, -1,
              1, -1, 1, 1,
              0, 2, 1, 1), 4, 4, byrow = TRUE)
b <- c(8, 1, 4, 16)
solve(A) %*% b

#------------------------------------------------------


lsodes()
ode.band()

A <- matrix(c(2, 1, 1, -1,
              2, 2, 0, -1,
              1, -1, 1, 1,
              0, 2, 1, 1), 4, 4, byrow = TRUE)
b <- c(8, 1, 4, 16)

#out2 <- lsodes(func = chemistry, y = b, parms = parms, times = times, inz = A, atol = atol,rtol = rtol)


####wzory Cramera
###rownanie:
  ###1x+2y=3
###4x+5y=6
###Rozwiazanie w R:
  a1<-as.integer(1)
b1<-as.integer(2)
c1<-as.integer(3)
a2<-as.integer(4)
b2<-as.integer(5)
c2<-as.integer(6)
temp=(a1*b2)-(a2*b1)
if(temp==0)
  print("nie da sie")
if(temp!=0)
{
  x=(c1*b2-c2*b1)/temp
  y=(a1*c2-a2*c1)/temp
}
###Wynik w zmiennych x,y analogicznie dla 3 zmiennych liczymy zmienna z, itd.
####Zadanie 5 Wykorzystujac funkcje jacobi dla metody iteracyjnej Jacobiego znalezc rozwiazanie ukladu rownan: 
  
  
 ### 4x-y=2 
###-x+4y-z=6
###-y+4z=2 
f1<-function(y,z) 
{ (2+y)/4 } 
f2<-function(x,z) 
{ (6+x+z)/4 } 
f3<-function(x,y) 
{ (2+y)/4 } 
x<-as.integer(1) 
y=x 
z=x
i=0 
while(i<20)
{ 
  i=i+1
  tempx=x 
  tempy=y
  tempz=z 
  tex=x
  tey=y 
  tez=z
  x=f1(tempy,tempz) 
  y=f2(tempx,tempz)
  z=f3(tempx,tempy) 
  if(x==1&&y==0.999997&&z==0.999992) 
    break 
  if(x==tex&&y==tey&&z==tez)
    break
} 
####wyniki rownania w zmiennych x,y,z.


