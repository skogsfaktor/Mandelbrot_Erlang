-module(color).
-compile(export_all). 

%5 sections represent different depths which correspond to different colors. 
convert(Depth, Max) ->
    F = Depth/Max,
    A = 4*F,
    X = trunc(A),
    Y = trunc(255*(A - X)),
    case X of 
	0 ->
	    {Y, 0, 0};
	1 ->
	    {255, Y, 0};
	2 ->
	    {255 - Y, 255, 0};
	3 ->
	    {0, 255, Y};
	4 ->
	    {0, 255 - Y, 255}
    end. 
