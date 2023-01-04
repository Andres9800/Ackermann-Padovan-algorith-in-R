library(lisp)
# 1) ------- RECURSIVA CLÁSICA Ackerman ----------------------------


A <- function ( n, m ) {
  if (m == 0) 
    return (0)
  if (n == 0) 
    return (2 * m)
  if (m == 1)
    return (2)
  
    return (A (n-1, A(n , m - 1)))
}

A (1 , 10)
A (2 , 4)
A (3 , 3)


# 2) ------- Sucesion de Padovan: ----------------------------

Pad <- function ( n ) {
  if (n < 3)
    return (1)
  return (Pad (n - 2) + Pad (n - 3) )
}

Padpr <- function ( i, n ) {
  if (i < n){
    print(Pad (i)) 
    Padpr (i+1, n)    
  }
}
Padprf <- function ( n ) {
  Padpr(0, n) 
}

Padprf( 10)
Padprf( 12)
Padprf( 21)

#recursion lineal

Plineal.iter <- function ( tras, ant, result, i , n ) {
  if (i >= n)
    return (tras)
  return (Plineal.iter (ant, result, (ant+tras), (i+1), n) )
}

PadLin <- function ( n ) {
  
    return (Plineal.iter (1, 1, 1, 0, n)) 
  
}
PadLinR <- function ( i, n ) {
  if (i < n){
    print(PadLin (i)) 
    PadLinR (i+1, n)    
  }
}
PadLinF <- function ( n ) {
  PadLinR (0, n)
}
PadLinF (10)
PadLinF( 12)
PadLinF( 21)



# 2.6) ------- ALGORITMO DE LUHN ----------------------------

pro<-function(para){if(length(para)==0){return(NULL)}else{
    (c(pro(cdr(para)),car(para)))
  }
}

funl<-function(entre,paral,len){if(length(paral)==0){entre} else  {
    if(len == 1){(funl(entre+car(paral),cdr(paral),len+1))
    } else{cur<-(car(paral)*2)
      if (cur>=10){(funl(entre+(cur-9),cdr(paral),len-1))
      } else{(funl(entre+cur,cdr(paral),len-1))
      }
    }
  }
}

Luhn<-function(n){
  if((funl(0,pro(n),1)%%10)==0){
    TRUE
  }  else{
    FALSE
  }
}
x <- list(4,9,9,2,7,3,9,8,7,1,6)
Luhn(x)


