# User defined functions

```
-- a user defined function is declared like this


increment = (a, a+1)


-- and called like this
x = (increment 3)


-- this produces the same result as (increment 3).
y = ((a, a+1) 3)


-- functions can have 0 parameters,
y = ((, 2))

-- Are functions closures?
b = 10
z = (, b+5)
-- should be 15
Print (z)

```


