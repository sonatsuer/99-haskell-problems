-- NO CODE HERE

-- Problem 38
-- Compare totient and phi.

{-
Obviously the algorithm which uses  prime factorization is
slower. Here are the resources used by map phi [1..3000]
and map totient [1..3000] in ghci 7.10.1 on a Lenovo G500 laptop:

map phi [1..3000]      --> (74.13 secs, 36,885,891,192 bytes)
map totient [1..3000]  --> (32.05 secs, 10,331,217,944 bytes)
-}
