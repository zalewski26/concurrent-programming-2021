package body graph is

	-- Ustawianie wartości kluczowych stałych
	procedure setConsts(n_const, d_const, h_const, max_delay_const: Integer) is
	begin
		n := n_const;
		d := d_const;
		h := h_const;
		max_delay := max_delay_const;
	end setConsts;

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
			if (i /= 0) then
				intVector.Append(tempVert.next, Integer(i - 1));
			end if;
			verticeVector.Append(verts, tempVert);
		end loop;

		counter := 0;
		while counter < d loop
			full := true;
			for i in 0..n-1 loop
			   	if (Integer(verts(i).next.Length) /= (n - 1)) then
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
				intVector.Append(verts(newNext).next, index);
				counter := counter + 1;
			end if;
		end loop;

		for i in 0..n-1 loop
			for j in 0..h-1 loop
			   hostVector.Append(verts(i).hosts, new host(j, i));
			end loop;
		  	verts(i).R.init(i, n);
		end loop;
	end graphGenerator;

	--Drukowanie grafu
	procedure printGraph is
	begin
		Printer.println("Graf:");
		for i in verts.First_Index .. verts.Last_Index loop
			Printer.print(i'Img & ": [");
			for j in verts(i).next.First_Index .. verts(i).next.Last_Index loop
				Printer.print(verts(i).next.Element (j)'Img);
			end loop;
			Printer.Println("]");
			Printer.Print("     Hosts: {");
			for j in verts(i).hosts.First_Index .. verts(i).hosts.Last_Index loop
				Printer.print(verts(i).hosts.Element(j).id'Img);
			end loop;
			Printer.println("}");
		end loop;
	end printGraph;

	procedure startHosts is
	begin	
		for i in 0..n-1 loop
			for j in 0..h-1 loop
				verts(i).hosts(j).start;
			end loop;
		end loop;
	end startHosts;


	protected body routingTable is
        procedure init(index : in Integer; size : in Integer) is
		begin
			id := index;
			for i in 0..n-1 loop
			   	if (index = i) then
			   		intVector.Append(nextHop, index);
					intVector.Append(cost, 0);
					intVector.Append(changed, 0);
			   	elsif (index < i) then
					intVector.Append(nextHop, index + 1);
					intVector.Append(cost, i - index);
					intVector.Append(changed, 1);
				else
					intVector.Append(nextHop, index - 1);
					intVector.Append(cost, index - i);
					intVector.Append(changed, 1);
			   	end if;
			end loop;

			for i in 0..Integer(verts(index).next.Length) - 1 loop
				  nextHop(verts(index).next(Integer(i))) := verts(Integer(index)).next(Integer(i));
				  cost(verts(index).next(Integer(i))) := 1;
			end loop;
		end init;

		procedure preparePack(result : out package_access) is
			toSend : Boolean := false;
			tempPack : package_access;
		begin
			tempPack := new pack(id);
			for i in 0..n-1 loop
			   	intVector.Append(tempPack.nextHop, 0);
				intVector.Append(tempPack.cost, 0);
				intVector.Append(tempPack.valid, 0);
			end loop;

			for i in 0..n-1 loop
			   tempPack.valid(i) := changed(i);
			   if (tempPack.valid(i) = 1) then
					tempPack.nextHop(i) := nextHop(i);
					tempPack.cost(i) := cost(i);
					toSend := true;
					changed(i) := 0;
			   end if;
			end loop;

			if (toSend) then
				result := tempPack;
			else
				result := null;
			end if;
		end preparePack;

		procedure changeTable(myPack : in package_access) is
		begin
			for i in 0..n-1 loop
			   if (myPack.valid(i) = 1) then
					if (myPack.cost(i) + 1 < cost(i)) then
						nextHop(i) := myPack.id;
						cost(i) := myPack.cost(i) + 1;
						changed(i) := 1;
						myFinish.change;
						Printer.Println("Wierzchołek" & id'Img & " aktualizuje ścieżkę do" & i'Img);
					end if;
			   end if;
			end loop;
		end changeTable;

		procedure print is
		begin
			for j in 0..n-1 loop
			  	Printer.Print(" {nextHop[" & j'Img & "] =" & nextHop.Element(j)'Img);
				Printer.Print(" cost[" & j'Img & "] =" & cost.Element(j)'Img & "}");
				Printer.newline;
		   end loop;
		end print;

		procedure getNext(dest : in Integer; result : out Integer) is
		begin
			result := nextHop(dest);
		end getNext;
		
    end routingTable;

	task body Sender is
		temp : package_access;
		pack_desc : Unbounded_String;
		empty : Unbounded_String;
	begin
		loop
			select
			   accept stop do
					null;
			   end stop;
			   exit;
			or
				delay (0.3 + Duration(Float((Integer(random(gen)) mod max_delay)) / 1000.0));
				verts(id).R.preparePack(temp);
				if (temp /= null) then
					for i in 0..Integer(verts(id).next.Length) - 1 loop
						recs(verts(id).next(i)).Receive(temp);
					end loop;
				end if;
			end select;
		end loop;
	end Sender;

	task body Receiver is 
		temp: package_access;
	begin
		loop 
			select	
				accept receive(item : in package_access) do
					temp := item;
				end;
				verts(id).R.changeTable(temp);
			or 
				terminate;	
			end select;
		end loop;	
	end Receiver;

	task body ForwReceive is 
		temp: stdPack_access;
	begin
		loop 
			select	
				accept Receive(item : in stdPack_access) do
					temp := item;
				end;
				verts(id).Queue.Enqueue(New_Item => temp);
			or 
				terminate;	
			end select;
		end loop;	
	end ForwReceive;

	task body ForwPass is
		temp: stdPack_access;
		next: Integer;
	begin
		loop
				delay 0.01;
				select
					verts(id).Queue.Dequeue(Element => temp);
				or 
					delay 3.0;
					exit;
				end select;	
				intVector.Append(temp.visited, id);
				if (temp.recv_router = id) then
					verts(id).hosts(temp.recv_host).Receive(temp);
				else 
					verts(id).R.getNext(temp.recv_router, next);
					forws(next).Receive(temp);
				end if;
		end loop;
	end ForwPass;

	task body host is 
		temp: stdPack_access;
		newPack: stdPack_access;
		pack_desc : Unbounded_String;
		pack_desc2 : Unbounded_String;
		empty : Unbounded_String;
		nextRouter : Integer;
		nextHost : Integer;
	begin
		select
		   	accept start do
				null;
		   	end;
			
			nextRouter := Integer(random(gen)) mod n;
			nextHost := Integer(random(gen)) mod h;
			loop
				if (nextRouter /= router_id or nextHost /= id) then
					exit;
				end if;  
				nextRouter := Integer(random(gen)) mod n;
				nextHost := Integer(random(gen)) mod h;
			end loop;

			newPack := new standard_pack(router_id, id, nextRouter, nextHost);
			forws(router_id).Receive(newPack);
			
			loop 
				select
					accept stop do
						null;
					end;
					exit;
				or
					accept receive(item : in stdPack_access) do
						temp := item;
					end;
					pack_desc := empty;
					Append(pack_desc, "(");
					Append(pack_desc, router_id'Img);
					Append(pack_desc, ", ");
					Append(pack_desc, id'Img);
					pack_desc2 := empty;
					Append(pack_desc2, "{ ");
					for i in 0..Integer(temp.visited.Length) - 1 loop
					   Append(pack_desc2, temp.visited.Element(i)'Img);
					end loop;
					Append(pack_desc2, " }");
					Printer.Println(To_String(pack_desc) & ") odebrał paczkę od (" & temp.send_router'Img & ", " & temp.send_host'Img & ") visited = " & To_String(pack_desc2));
					delay 0.3;
					forws(router_id).Receive(new standard_pack(router_id, id, temp.send_router, temp.send_host));
				or 
					terminate;	
				end select;
			end loop;
		or
			terminate; 
		end select;
		
	end host;

	task body Finish is
	begin
		loop
		   select
				accept change do
				   null;
				end change;
		   or
		   		delay 1.0;
				   for i in 0..n-1 loop
						sends(i).stop;
					end loop;
					Printer.Println("Routing zakończony");
					delay 5.0;
					for i in 0..n-1 loop
						for j in 0..h-1 loop
							verts(i).hosts(j).stop;
						end loop;
					end loop;
				exit;
		   end select;
		end loop;
	end Finish;

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
			   	accept stop do
					null;
				end;
				exit;
			or	
				terminate;			  
		   	end select;
		end loop;
	end Printer;	
end graph;	