doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

length' xs = sum [1 | _ <- xs]

