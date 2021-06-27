package body graph is

	-- Ustawianie wartości kluczowych stałych
	procedure setConsts(n_const, d_const, k_const, max_delay_const : Integer) is
	begin
		n := n_const;
		d := d_const;
		k := k_const;
		max_delay := max_delay_const;
	end setConsts;

	--Generowanie grafu działa na zasadzie utworzenia połączeń między wierzchołkami 0->1->2->...->n-1,
	--a następnie na losowym wybraniu d dodatkowych połączeń.
	procedure graphGenerator is
		tempVert : vertice_access;
		counter : Integer;
		index : Integer;
		newNext : Integer;
		exists : Boolean;
		full : Boolean;
	begin
		Reset(gen);

		for i in 0..n-1 loop
			tempVert := new vertice(i);
			if (i /= n - 1) then
				intVector.Append(tempVert.next, Integer (i + 1));
			end if;
			verticeVector.Append(verts, tempVert);
			fwdVector.Append(fwds, new Forwarder(tempVert));
		end loop;

		counter := 0;
		while counter < d loop
			full := true;
			for i in 0..n-1 loop
			   	if (Integer(verts(i).next.Length) /= (n - 1 - i)) then
					full := false;
					exit;
				end if;
			end loop;
			exit when full;

			exists := false;
			index := Integer(random(gen)) mod (n - 1);
			newNext := index + 1 + (Integer(random(gen)) mod (n-1-index));

			for i in verts(index).next.First_Index .. verts(index).next.Last_Index loop
				if verts(index).next(i) = newNext then
					exists := true;
					exit;
				end if;
			end loop;
				
			if (not exists) then
				intVector.Append(verts(index).next, newNext);
				counter := counter + 1;
			end if;
			
		end loop;

		for i in 0..k-1 loop
			packageVector.Append(packs, new pack(i));
		end loop;
	end graphGenerator;

	--Drukowanie grafu
	procedure printGraph is
	begin
		Printer.println("Graf:");
		for i in verts.First_Index .. verts.Last_Index loop
			Printer.print(i'Img & "-> [");
			for j in verts(i).next.First_Index .. verts(i).next.Last_Index loop
				Printer.print(verts(i).next.Element (j)'Img);
			end loop;
			Printer.println("]");
		end loop;
	end printGraph;

	--Funkcja wysyłająca, która w odstępach [0,max_delay) milisekund wysyła nowy pakiet do źródła.
	task body Sender is
	begin
		for i in 0..k-1 loop
			Printer.println("Pakiet " & packs(i).id'Img & " jest wysyłany");
			fwds(0).Receive(packs(i));
			delay Duration(Float((Integer(random(gen)) mod max_delay)) / 1000.0); 
		end loop ;
	end Sender;
	
	
	-- Funkcja forwardera (wierzchołka), który w danym momencie przyjmuje jeden pakiet, 
	-- a po odczekaniu [0,max_delay) milisekund przesyła go dalej.
	task body Forwarder is
	temp : package_access;
	begin
		loop
			select
				accept Receive(item : in package_access) do
					temp := item;
				end; 
				Printer.println("Pakiet " & Integer'Image(temp.id) & " jest w wierzchołku " & vert.id'Img );
				intVector.Append(temp.visited, vert.id);
				intVector.Append(vert.packages, temp.id);
				delay Duration(Float((Integer(random(gen)) mod max_delay)) / 1000.0); 
				if (vert.id /= n - 1) then
					fwds(vert.next(Integer(random(gen)) mod Integer(vert.next.Length))).Receive(temp);
				else
					myReceiver.Receive(temp);	
				end if;
			or
				terminate;
			end select;
		end loop;
	end Forwarder;

	--Funkcja ujścia, które w odstępach [0,max_delay) milisekund odbiera (jeśli jest to możliwe) nowy pakiet od ostatniego wierzchołka.
	task body Receiver is 
		temp: package_access;
		counter : Integer := 0;
	begin
		loop 
			select
				accept receive(item : in package_access) do
					temp := item;
				end;
				Printer.println("Pakiet " & Integer'Image(temp.id) & " został odebrany");
				counter := counter + 1;
				if counter = k then
					exit;
				end if;
				delay Duration(Float((Integer(random(gen)) mod max_delay)) / 1000.0);
			or 
				terminate;	
			end select;
		end loop;	
	end Receiver;

	--Funkcja wykorzystana do synchronizacji drukowania komunikatów
	task body Printer is
	begin
		loop
		   	select
			  	accept print(text: String) do
				   Put(text);
				end print;	
			or
				accept println(text: String) do
				   Put_Line(text);
				end println;
			or
				accept newline do
				   	New_Line(1);
				end newline;			
		   	or
				terminate;			  
		   	end select;
		end loop;
	end Printer;	
end graph;	