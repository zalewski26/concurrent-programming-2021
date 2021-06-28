with graph;
with Ada.Command_Line;
with Ada.Strings;
use Ada.Command_Line;
use graph;

procedure main is    
	max_delay : Integer := 500;
begin
	if Argument_Count /= 2 then
		Printer.println("./main $n $d");
		return;
	end if;

	declare
	   	n: Integer := Integer'Value (Argument(1));
		d: Integer := Integer'Value (Argument(2));
		
	begin
	   	setConsts(n, d, max_delay);
		graphGenerator;
		printGraph;

		Printer.newline;
		Printer.Println("Wierzchołki:");
		for i in 0..n-1 loop
		   Printer.Println(verts(i).id'Img & ":");
		   verts(i).R.print;
		   Printer.newline;
		end loop;
		
		for i in 0..n-1 loop
		   recVector.Append(recs, new Receiver(i));
		   sendVector.Append(sends, new Sender(i));
		end loop;

		loop
			if myFinish'Terminated then 
				exit;
			end if;
		end loop;

		Printer.newline;
		Printer.Println("Wierzchołki:");
		for i in 0..n-1 loop
		   Printer.Println(verts(i).id'Img & ":");
		   verts(i).R.print;
		   Printer.newline;
		end loop;
	end;	
end main;