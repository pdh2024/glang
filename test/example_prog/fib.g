fib = n. cond ((n == 0) | (n == 1)) n (fib (n-1) + fib (n-2)) 

main = print (fib 9)