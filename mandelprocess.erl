-module(mandelprocess). 
-compile(export_all). 

demo() ->
%Snygg: (-0.15, 0.85, -0.13), (-0.165,0.85,-0.155),(-0.1635,0.8484,-0.156), (-0.162,0.84745,-0.157), (-0.138,0.85,-0.132), (-0.1362,0.8408,-0.132)
    small(-2.6, 1.2, 1.6).

%X is upper left corner. 
%X1 is the upper right corner.
%Y is the distance from 0i to Yi.  
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
%Trans translates a pixel to a complex number. 
    Trans = fun(W, H) ->
		    cmplx:new(X + K*(W-1), Y-K*(H-1))
	    end,
    rows(Width, Height, Trans, Depth, []).

rows(Width, Height, Trans, Depth, Image) ->
%the 0 is used for iterating over the rectangle. 
    rows(Width, Height, Trans, Depth, Image, 0).

%When we have iterated through all the rows we should return the image.
rows(_, Height, _, _, Image, IterHeight) when IterHeight >= Height ->
%cutOrder will cut off the RowOrder. 
    cutOrder(rowConcat(Height, Image), []);
%IterHeight is the current row we are on. 
rows(Width, Height, Trans, Depth, Image, IterHeight) ->
    Pid = self(),
%spawn a process for every row. 
    spawn(fun() -> row(Width, IterHeight, Trans, Depth, [], 0, Pid) end),
    rows(Width, Height, Trans, Depth, Image, IterHeight + 1).

%rowConcat will return a sorted list with tuples on the form {RowOrder, Row}. This is no. 
rowConcat(0, Image) ->
    Image;
rowConcat(Height, Image) ->
%Insertionsort after each row is added to the complete image. Nice!.    
%This function takes messages from the row processes on the form {RowOrder, Row} and adds each completed row to a list. This list is then insertionsorted for every concatenation.  
    receive 
	{RowOrder, Row} ->
	    rowConcat(Height - 1, isort([{RowOrder, Row}] ++ Image))
    end. 

%cuts of thee RowOrder from each row and returns a finished image. 
cutOrder([], Result) ->
    Result;
cutOrder([{_, Row}|T], Result) ->
    cutOrder(T, [Row]++Result).


isort(List) ->                           
    isort(List, []). 
    
isort([], Sorted) ->                    %Insertion sort. 
    Sorted;
isort(List, Sorted) ->                  %Sorted should increase in size until List is empty. Sorted is then returned to the user. 
    [H|T] = List,
    isort(T, insert(H, Sorted)). 
    
%sorts a list of tuples in the form {RowOrder, Row} in ascending RowOrder. 
insert(Element, []) ->                  %insert Element in correct place in List. 
    [Element];
insert({RowOrder, Row}, List) ->
    [{RowOrderList, RowList}|T] = List,
    case RowOrder >= RowOrderList of
	true -> [{RowOrder, Row}]++List;         %if Element is smaller than the first element of the sorted list. 
	false ->[{RowOrderList, RowList}]++insert({RowOrder, Row}, T)  %If Element is not smaller than the first element of the sorted list. 
    end. 

%When we have iterated through a particular row we should return the row(List of colors). 
row(Width, IterHeight, _, _, Row, IterWidth, Pid) when IterWidth >= Width ->
    Pid ! {IterHeight, Row};
row(Width, IterHeight, Trans, Depth, Row, IterWidth, Pid) ->
%Trans calculates a pixels complex number based on its position(width and height) 
    C = Trans(IterWidth, IterHeight),
%The complex number is converted into a tuple {R, G, B} by first making it into a depth and then into a color. 
    Color = color:convert(brot:mandelbrot(C, Depth), Depth - 1),
%The color for a pixel is added to the result list. 
    NewRow = Row ++ [Color],
    row(Width, IterHeight, Trans, Depth, NewRow, IterWidth + 1, Pid).


