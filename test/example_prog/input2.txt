foo = x. y. x * x + y

roo = a. b. (foo a (foo a b))

main = print (roo 3 5)