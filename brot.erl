-module(brot).
-compile(export_all). 

mandelbrot(C, M) ->
    Z0 = cmplx:new(0,0),
    I = 0,
    test(I, Z0, C, M).

%Should return 0 if Number of iterations so far = maximum number of iterations.
test(M, _, _, M) ->	
    0;
%M = maximum number of iterations. 
%I = number of iterations so far. 
%C = point on the map. 
%Z0 = starter value. 
test(I, Z0, C, M) ->
%Return number of iterations so far if abs(Z0) > 2
    case cmplx:abs(Z0) > 2 of 
	true ->
	    I;
	false ->
%original, Cmplx:add(cmplx:sqr(Z0), C). 
	    NextZ = cmplx:add(cmplx:sqr(Z0), C),
	    test(I + 1, NextZ, C, M)
    end.

