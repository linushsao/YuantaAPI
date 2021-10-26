test.a <-"A"

fun1 <-function()
{
  print(paste("ORIGION GLOBAL VAR. IN FUN1:", test.a))
  test.a <-"B"
  print(paste("MODIFIED IN FUN1 :", test.a))
        
  fun2(test.a)
  fun3()
}

fun2 <-function(x)
{
  print(paste("IN FUN2 drop by FUN1 :", x))
}

fun3 <-function()
{
  print(paste("IN FU3 :", test.a))
}

fun1()
