pair = x. y. f. f x y

fst = p. p (x. y. x)

snd = p. p (x. y. y)

p1 = pair 3.14 5

out1 = fst p1

out2 = snd p1

main = print out2