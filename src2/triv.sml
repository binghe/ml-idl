(*
	Generated by IDL: 1.4 92/10/30 13:09:32
*)



datatype Widget = Widget of address;
fun write_Widget(Widget address'V)=write_address(address'V);
fun read_Widget() = Widget(read_address());

datatype Window = Window of address;
fun write_Window(Window address'V)=write_address(address'V);
fun read_Window() = Window(read_address());

datatype Pos = Pos of short;
fun write_Pos(Pos short'V)=write_short(short'V);
fun read_Pos() = Pos(read_short());

datatype Dim = Dim of int;
fun write_Dim(Dim int'V)=write_int(int'V);
fun read_Dim() = Dim(read_int());

datatype String = String of address;
fun write_String(String address'V)=write_address(address'V);
fun read_String() = String(read_address());

datatype Str = Str of string;
fun write_Str(Str string'V)=write_string(string'V);
fun read_Str() = Str(read_string());

fun WidgetToWindow(a:Widget)=
( write_int 8;
 write_Widget(a);
 (read_Window())
)

fun WidgetToDim(w:Widget)=
( write_int 7;
 write_Widget(w);
 (read_Dim())
)

fun WidgetToDimPos(w:Widget)=
( write_int 6;
 write_Widget(w);
 (read_Dim(),read_Pos())
)



fun WidgetToDimPos1(w:Widget)=
( write_int 5;
 write_Widget(w);
 (read_Dim(),read_Pos())
)

fun WidgetToDimPos2(w:Widget)=
( write_int 4;
 write_Widget(w);
 (read_Dim(),read_Pos())
)

fun Echo(s:String)=
( write_int 3;
 write_String(s);
 (read_Str())
)

fun EchoStr(s:Str)=
( write_int 2;
 write_Str(s);
 (read_Str())
)



	fun StartServer args =
	(idlbase.startserver "foo" args;
	 if GetSignature() = Signature then
	    (outputc std_err "[Wrong server]\n";
	     raise Interrupt
	    )
	 else

	)


val Signature =
"WidgetToWindow:Widget->Window;WidgetToDim:Widget->Dim;WidgetToDimPos:Widget->Dim,Pos;WidgetToDimPos1:Widget->Dim,Pos;WidgetToDimPos2:Widget->Dim,Pos;Echo:String->Str;EchoStr:Str->Str;"
fun GetSignature()=
( write_int 1;
 
 (read_string())
)