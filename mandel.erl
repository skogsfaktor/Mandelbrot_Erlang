-module(mandel). 
-compile(export_all). 

demo() ->
    small(-2.6, 1.2, 1.6).

%{X, Y} is upper left corner. 
%X1 is right corner. 
%K is the distance between each pixel. 
%Depth = number of iterations per pixel. 
small(X,Y,X1) ->
    Width = 1920,
    Height = 1080,
    K = (X1 - X)/Width,
    Depth = 512,
    T0 = now(),
    Image = mandelbrot(Width, Height, X, Y, K, Depth),
    T = timer:now_diff(now(), T0),
    io:format("picture generated in ~w ms~n", [T div 1000]),
    ppm:write("fractal.ppm", Image).

mandelbrot(Width, Height, X, Y, K, Depth) ->
    Trans = fun(W, H) ->
		    cmplx:new(X + K*(W-1), Y-K*(H-1))
	    end,
    rows(Width, Height, Trans, Depth, []).

rows(Width, Height, Trans, Depth, Image) ->
%the 0 is used for iterating over the rectangle. 
    rows(Width, Height, Trans, Depth, Image, 0).

%When we have iterated through all the rows we should return the image.
rows(_, Height, _, _, Image, IterHeight) when IterHeight >= Height ->
    Image;
%IterHeight is the current row we are on. 
rows(Width, Height, Trans, Depth, Image, IterHeight) ->
%We send the current row we are on to row(). 
    NewImage = Image ++ [row(Width, IterHeight, Trans, Depth, [], 0)],
    rows(Width, Height, Trans, Depth, NewImage, IterHeight + 1).

%When we have iterated through a particular row we should return the row(List of colors). 
row(Width, _, _, _, Row, IterWidth) when IterWidth >= Width ->
    Row;
row(Width, IterHeight, Trans, Depth, Row, IterWidth) ->
%Trans calculates a pixels complex number based on its position(width and height) 
    C = Trans(IterWidth, IterHeight),
%The complex number is converted into a tuple {R, G, B} by first making it into a depth and then into a color. 
    Color = color:convert(brot:mandelbrot(C, Depth), Depth - 1),
%The color for a pixel is added to the result list. 
    NewRow = Row ++ [Color],
    row(Width, IterHeight, Trans, Depth, NewRow, IterWidth + 1).


