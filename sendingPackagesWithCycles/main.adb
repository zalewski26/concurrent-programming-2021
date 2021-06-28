with graph;
with Ada.Command_Line;
with Ada.Strings;
use Ada.Command_Line;
use graph;

procedure main is    
	max_delay : Integer := 500;
	trap_delay : Integer := 3000;
begin
	if Argument_Count /= 6 then
		Printer.println("./main $n $d $b $k $h $kłusownik(0 => wyłączony, 1 => włączony)");
		return;
	end if;

	declare
	   	n: Integer := Integer'Value (Argument(1));
		d: Integer := Integer'Value (Argument(2));
		b: Integer := Integer'Value (Argument(3));
		k: Integer := Integer'Value (Argument(4));
		h: Integer := Integer'Value (Argument(5));
		poacherEnabled : Integer := Integer'Value (Argument(6));
		
	begin
	   	setConsts(n, d, b, k, h, max_delay, trap_delay);
		graphGenerator;
		printGraph;
		Printer.println("Przebieg pakietów:");

		declare
			mySender : Sender;
		begin
			if poacherEnabled /= 0 then
				myPoacher.Start;
			end if;

			loop
				if myThrash'Terminated then 
					myPoacher.Stop;
					exit;
				end if;
			end loop;
				
			Printer.newline;
			Printer.println("Pakiety:");
			for i in 0..k-1 loop
				Printer.print(packs(i).id'Img & " odwiedził: [");
				for x of packs(i).visited loop
					Printer.print(x'Img & " "); 
				end loop;
				Printer.println("]");
			end loop;

			Printer.newline;
			Printer.println("Wierzchołki:");
			for i in 0..n-1 loop
				Printer.print(verts(i).id'Img & " przepuścił: [");
				for x of verts(i).packages loop
					Printer.print(x'Img & " "); 
			end loop;
			Printer.println("]");
			end loop;
		end;	
	end;
end main;