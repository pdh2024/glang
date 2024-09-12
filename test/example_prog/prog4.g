foo = x. y. x * y

roo = x. y. x + y

too = a. b. (foo a b) + (roo a b)

main = print (too 5 6)