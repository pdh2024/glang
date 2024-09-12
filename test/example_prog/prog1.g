cons = a. b. f. f a b

head = l. l (x. y. x)
tail = l. l (x. y. y)

inf = x. cons x (inf x)

main = print (head (tail (inf 4)))

