--Partial application of function
--sum = foldl (+) 0
--sum = \xs -> foldl (+) 0 xs

--Function Generators
gen_add_n = \n -> \x -> x+n
add_3 = gen_add_n 3
add_7 = gen_add_n 7

gen_op_n = \op n -> \x -> x `op` n
add_4 = gen_op_n (+) 3
mult_7 = gen_op_n (*) 7
