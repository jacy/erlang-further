%% @author jacy
%% @doc @todo Add description to mtypes.


-module(mtypes).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-import(myio,[p/1,p/2]).

boolean() ->
	p(not true),
	p(true or false),
	p(true and false),
	p(not (true and false)),
'Ending...'.
	
equality_test()->
	%% As a general rule of thumb, you should always start by using =:= and =/=, 
	%% and switch to == and /= only when you know you do not need exact equality. 
	p("5 == 5.0 is ", 5 == 5.0),
	p("5 /= 5.0 is ",5 /= 5.0), % key an eye on /=
	p("5 =:= 5.0 is ", 5 =:= 5.0),
	p("5 =/= 5.0 is ",5 =/= 5.0),
	
	p("1 > 5 is ",1 > 5),
	p("1 < 5 is ",1 < 5),
	p("1 >= 5 is ",1 >= 5),
	p("1 =< 5 is ",1 =< 5), % key an eye on =<

	%% The terms true and false are atoms, but they are integrated well enough into the language that you shouldn’t have a problem with them.
	%%  number < atom < reference < fun < port < pid < tuple < list < bit string
	p("0 == false is ", 0 == false),
	p("1 < false is ", 1 < false),
	p("*******Remember that:number < atom < reference < fun < port < pid < tuple < list < bit string"),
'Ending...'.

%% Erlang doesn’t let you add two operands of different types, it will let you compare them. 
tolerant_test() ->
	p(5 =:= true), % =:= accept different types comparsion
	try p(5 + some_atom) % + can not take different types
	catch error:Err -> p({error, Err})
	end,
%% Why does it refuse different types in some operations but not others? 
%% This is because the creators of Erlang thought pragmatism beats theory and decided it would be great to be 
%% able to simply write things like general sorting algorithms that could order any terms. 
%% It’s there to make your life simpler and can do so the vast majority of the time.
'Ending...'.


list() ->
	p([1,2,3] ++ [4,5]),
 	p([1,2,3,4,5] -- [1,2,3]),
 	p("[2,4,2] -- [2,4] = " , [2,4,2] -- [2,4]),
	p([2,4,2] -- [2,4,2]),

	%% Both ++ and -- are right-associative. 
	%% This means the elements of many -- or ++ operations will be done from right to left
	p("[1,2,3] -- [1,2] -- [3] = ",[1,2,3] -- [1,2] -- [3]),
	
	%% built-in functions (BIFs)
	p("head of [1,2,3,4] = ",hd([1,2,3,4])),
 	p("tail of [1,2,3,4]= ",tl([1,2,3,4])),
'Ending...'.

%% Erlang bit syntax encloses binary data between << and >> and splits it in readable segments; 
%% each segment is separated by a comma.
binary() ->
	Color = 16#F09A29, % hexadecimal notation
	Pixel = <<Color:24>>, % Put the binary values of #F09A29 on 24 bits of space (red on 8 bits, green on 8 bits, and blue also on 8 bits) in the variable Pixel
	p(Pixel),
	
	%% A segment is a sequence of bits of a binary (not necessarily on a byte boundary, although this is the default behavior).
	%% so <<213,45,132>> is equal to <<213:8,45:8,132:8>>
	Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>, %% Have 12 segments, each segment default has one byte
	
	<<Pix1:24, _Pix2:24, _Pix3:24, _Pix4:24>> = Pixels, % Patten match
	<<R:8, _G:8, _B:8>> = <<Pix1:24>>,
	p(R),

	%% only want the first color from the start
	%% Rest/binary is a specific notation that lets you say that whatever is left in the binary, whatever length it is, is put into the Rest variable.
	%% So <<Pattern, Rest/binary>> is to binary pattern matching what [Head | Tail] is to list pattern matching.
	<<FirstColor:8,_Rest/binary>> = Pixels, % Binary pattern matching
	p(R == FirstColor),
	
	%% Erlang allows more than one way to describe a binary segment. The following are all valid:
		% Value
		% Value:Size 
		% Value/TypeSpecifierList 
		% Value:Size/TypeSpecifierList
	%% Size is always in bits when no TypeSpecifierList is defined. TypeSpecifierList represents one or more of the following, separated by a hyphen (-):
		% 1) Type: The possible values are integer, float, binary, bytes, bitstring, bits, utf8, utf16, and utf32. When no type is specified, Erlang assumes an integer type.
	   		% This represents the kind of binary data used. Note that bytes is shorthand for binary, and bits is shorthand for bitstring.
		% 2) Signedness: The possible values are signed and unsigned. The default is unsigned. This only matters for matching when the type is integer.
		% 3) Endianness The possible values are big, little, and native. By default, endianness is set to big, as it is the standard used in network protocol 
			% encodings. Endianness only matters when the type is integer, utf16, utf32, or float. 
		% 4) Unit: This is written as unit:Integer. The unit is the size of each segment. The allowed range is 1 to 256.
			% It is set by default to 1 bit for integer, float, and bitstring types, and to 8 bits for binary. 
			% The default size of a data type can be changed by combining dif- ferent parts of a binary. 
			% As an example, <<25:4/unit:8>> will encode the number 25 as a 4-byte integer, or <<0,0,0,25>> in its graphical representation. 
			% <<25:2/unit:16>> will give the same result, and so will <<25:1/unit:32>>. Erlang will generally accept <<25:Size/unit:Unit>> and 
			% multiply Size by Unit to figure out how much space it should take to represent the value. Again, the result of this should be divisible by 8.
	<<X1/unsigned>> = <<-44>>,p(X1),
	<<X2/signed>> = <<-44>>,p(X2),
	<<X2/integer-signed-little>> = <<-44>>,p(X2),
	<<N:8/unit:1>> = <<72>>,p(N),
	<<N/integer>> = <<72>>,p(N),
	<<Y:4/little-unit:8>> = <<72,0,0,0>>,p(Y),
'Ending'.

%% The only change in syntax from regular list comprehensions is the <-, which becomes <= for binary generators, and using binaries (<<>>) instead of lists ([]).
binary_comprehension() ->
	<< <<X>> || <<X>> <= <<1,2,3,4,5>>, X rem 2 == 0>>,
	Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>,
	RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ],
	p(RGB),
	<< <<R:8, G:8, B:8>> || {R,G,B} <- RGB >>, % doing opposite
'Ending'.

	

%% ====================================================================
%% Internal functions
%% ====================================================================


