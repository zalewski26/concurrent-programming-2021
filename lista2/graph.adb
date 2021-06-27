package body graph is

	-- Ustawianie wartości kluczowych stałych
	procedure setConsts(n_const, d_const, b_const, k_const, h_const, max_delay_const, trap_delay_const : Integer) is
	begin
		n := n_const;
		d := d_const;
		b := b_const;
		k := k_const;
		h:= h_const;
		max_delay := max_delay_const;
		trap_delay := trap_delay_const;
	end setConsts;

	--Generowanie grafu działa na zasadzie utworzenia połączeń między wierzchołkami 0->1->2->...->n-1,
	--a następnie na losowym wybraniu d dodatkowych krawędzi postaci (i,j) oraz b dodatkowych krawędzi postaci (j,i), dla i < j.
	--W przypadku zbyt dużego d zostaną wygenerowane wszystkie możliwe skróty postaci (i,j). Analogicznie z b i (j,i).
	procedure graphGenerator is
		tempVert : vertice_access;
		counter : Integer;
		index : Integer;
		newNext : Integer;
		exists : Boolean;
		full : Boolean;
		tempPack : package_access;
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

		counter := 0;
		while counter < b loop
			full := true;
			for i in 0..n-1 loop
			   	if (Integer(verts(i).temp.Length) /= i) then
					full := false;
					exit;
				end if;
			end loop;
			exit when full;

			exists := false;
			index := 1 + Integer(random(gen)) mod (n - 1);
			newNext := Integer(random(gen)) mod (index);

			for i in verts(index).temp.First_Index .. verts(index).temp.Last_Index loop
				if verts(index).temp(i) = newNext then
					exists := true;
					exit;
				end if;
			end loop;
				
			if (not exists) then
				intVector.Append(verts(index).temp, newNext);
				counter := counter + 1;
			end if;
		end loop;

		for i in 0..n-1 loop
		   intVector.Append(verts(i).next, verts(i).temp);
		end loop;

		for i in 0..k-1 loop
			tempPack := new pack(i);
			tempPack.ttl := h;
			packageVector.Append(packs, tempPack);
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

	--Funkcja wysyłającego, który w odstępach [0,max_delay) milisekund wysyła nowy pakiet do źródła.
	task body Sender is
	begin
		for i in 0..k-1 loop
			Printer.println("Pakiet " & packs(i).id'Img & " jest wysyłany");
			fwds(0).Receive(packs(i));
			delay Duration(Float((Integer(random(gen)) mod max_delay)) / 1000.0); 
		end loop ;
	end Sender;
	
	--Funkcja forwardera (wierzchołka), który w danym momencie przyjmuje pakiet lub pułapkę na pakiet,
	--a po odczekaniu [0,max_delay) milisekund próbuje przesłać pakiet dalej.
	--W przypadku natrafienia na pułapkę lub przekroczenia TTL pakiet przepada.
	task body Forwarder is
		temp : package_access;
		trap_counter : Integer := 0;
		active_trap : Boolean := false;
	begin
		loop
			select
				accept Receive(item : in package_access) do
					temp := item;
					if (trap_counter > 0) then
						active_trap := true;
						trap_counter := trap_counter - 1;
					else
						active_trap := false;	
					end if;		
				end; 
				Printer.println("Pakiet " & Integer'Image(temp.id) & " jest w wierzchołku " & vert.id'Img );
				Integer_Sets.Include(temp.visited, vert.id);
				Integer_Sets.Include(vert.packages, temp.id);
				delay Duration(Float((Integer(random(gen)) mod max_delay)) / 1000.0); 

				if active_trap then
					Printer.println("Pakiet " & Integer'Image(temp.id) & " wpadł w pułapkę w wierzchołku " & vert.id'Img );
					myThrash.Throw;
				elsif temp.ttl = 0 then
					Printer.println("Pakiet " & Integer'Image(temp.id) & " - śmierć w wierzchołku " & vert.id'Img );
					myThrash.Throw;
				else
					passToNext(vert, temp);
					temp.ttl := temp.ttl - 1;
				end if;		
			or
				accept Trap do
					trap_counter := trap_counter + 1;
				end;	
			or
				terminate;
			end select;
		end loop;
	end Forwarder;

	--Funkcja przesłania pakietu, która losuje potencjalnego odbiorcę i próbuje wysłać do niego pakiet. 
	--W przypadku gdy nie jest to możliwe, proces jest powtarzany rekurencyjnie aż do skutku.
	procedure passToNext(vert : vertice_access; item : package_access) is
		rand : Integer;
		done : Boolean := false;
	begin
		rand := Integer(random(gen)) mod (Integer(vert.next.Length) + 1);
		if (rand = Integer(vert.next.Length)) then
			if (vert.id = n - 1) then
				myReceiver.Receive(item);
				done := true;
			else
				rand := Integer(random(gen)) mod Integer(vert.next.Length);
			end if;
		end if;

		if not done then
			select
			   fwds(vert.next(rand)).Receive(item);
			or
				delay 0.001;
				passToNext(vert, item);   
			end select;
		end if;
	end passToNext;

	--Funkcja kłusownika, który co pewien czas wysyła pułapkę do losowego wierzchołka
	task body Poacher is
		rand : Integer;
	begin
		select
		   	accept Start do
			   null;
			end;   
			loop
				select
					accept Stop do
						null;
					end;
					exit;
				or
					delay Duration(Float((Integer(random(gen)) mod max_delay)) / 1000.0); 
					rand := Integer(random(gen)) mod Integer(fwds.Length);
					fwds(rand).Trap;
					Printer.println("Wysłano pułapkę do " & rand'Img);
				end select;
			end loop;
		or
			accept Stop do
				null;
			end;
		or
			terminate;		   
		end select;
	end Poacher;

	--Funkcja odbiorcy, który co [0,max_delay) milisekund próbuje odebrać pakiet z ujścia.
	task body Receiver is 
		temp: package_access;
	begin
		loop 
			select
				accept receive(item : in package_access) do
					temp := item;
				end;
				Printer.println("Pakiet " & Integer'Image(temp.id) & " został odebrany");
				myThrash.Throw;
				delay Duration(Float((Integer(random(gen)) mod max_delay)) / 1000.0);
			or 
				terminate;	
			end select;
		end loop;	
	end Receiver;

	--Funkcja, która zbiera zużyte pakiety.
	task body Thrash is
		counter : Integer := 0;
	begin
		loop
		   	select
				accept Throw do
					counter := counter + 1;
				end;	
				if counter = k then
					exit;
				end if; 
		   	or
			 	terminate; 
		   end select;
		end loop;
	end Thrash;

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