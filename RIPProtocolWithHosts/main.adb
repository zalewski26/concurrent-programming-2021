with graph;
with Ada.Command_Line;
with Ada.Strings;
use Ada.Command_Line;
use graph;

procedure main is    
	max_delay : Integer := 1000;
begin
	if Argument_Count /= 3 then
		Printer.println("./main $n $d $h (hosts per router)");
		return;
	end if;

	declare
	   	n: Integer := Integer'Value (Argument(1));
		d: Integer := Integer'Value (Argument(2));
		h: Integer := Integer'Value (Argument(3));
		
	begin
	   	setConsts(n, d, h, max_delay);
		graphGenerator;
		printGraph;
		
		for i in 0..n-1 loop
		   recVector.Append(recs, new Receiver(i));
		   sendVector.Append(sends, new Sender(i));
		   forwVector.Append(forws, new ForwReceive(i));
		   forwVector2.Append(forws2, new ForwPass(i));
		end loop;
		startHosts;

		loop
			if myFinish'Terminated then 
				exit;
			end if;
		end loop;

		Printer.newline;
		Printer.Println("Tablica routingu:");
		for i in 0..n-1 loop
		   Printer.Println(verts(i).id'Img & ":");
		   verts(i).R.print;
		   Printer.newline;
		end loop;
	end;	
end main;