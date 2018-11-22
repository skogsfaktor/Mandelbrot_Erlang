-module(cmplx).
-compile(export_all).

new(R, I) ->
    {R, I}.

add({R1, I1}, {R2, I2}) ->
    {R1+R2, I1+I2}. 

sqr({R, I}) ->
    {R*R-I*I, 2*R*I}. 

abs({R, I}) -> 
    math:sqrt(R*R + I*I).
