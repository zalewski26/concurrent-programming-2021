with graph;
with Ada.Command_Line;
with Ada.Strings;
use Ada.Command_Line;
use graph;

procedure main is      
begin
	if Argument_Count /= 4 then
		Printer.println("./main $liczbaWierzchołków $liczbaSkrótów $liczbaPakietów $maxOpóźnienieMs");
		return;
	end if;

	declare
	   	n: Integer := Integer'Value (Argument(1));
		d: Integer := Integer'Value (Argument(2));
		k: Integer := Integer'Value (Argument(3));
		max_delay: Integer := Integer'Value (Argument(4));
	begin
	   setConsts(n, d, k, max_delay);
		graphGenerator;
		printGraph;
		Printer.println("Przebieg pakietów:");

		declare
			mySender : Sender;
		begin
			loop
				if myReceiver'Terminated then 
					exit;
				end if;
			end loop;

			Printer.newline;
			Printer.println("Pakiety:");
			for i in 0..k-1 loop
				Printer.print(packs(i).id'Img & " odwiedził: [");
				for j in 0..Integer(packs(i).visited.Length) - 1 loop
					Printer.print(packs(i).visited.Element (j)'Img & " "); 
				end loop;
				Printer.println("]");
			end loop;

			Printer.newline;
			Printer.println("Wierzchołki:");
			for i in 0..n-1 loop
				Printer.print(verts(i).id'Img & " przepuścił: [");
				for j in 0..Integer(verts(i).packages.Length) - 1 loop
					Printer.print(verts(i).packages.Element (j)'Img & " "); 
			end loop;
			Printer.println("]");
			end loop;
		end;
	end;
end main;