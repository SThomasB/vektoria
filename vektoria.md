

assign:
         [identifier]
    `then` [=]
    `then` [expression]

expression:
    lambda `or` bracket `or` binary expression

binary expression:...


function application:
    ( `then` [expression] [expressions...] `then` )
    i.e (callee arguments...)
callee:
