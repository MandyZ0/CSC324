-- f x =
--     if x == 0
--     then
--         10
--     else if x == 1
--     then
--         20
--     else
--         x + 30

f 0 = 10
f 1 = 20
f x = x + 30


-- g lst =
--     if null lst
--     then
--         10
--     else
--         let x = head lst
--             xs = tail lst
--         in
--             x + length xs

g [] = 10
g (x:xs) = x + length xs


-- Using cons in prefix form rather than the more conventional infix.
g1 [] = 10
g1 ((:) x xs) = x + length xs
