with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with ada.numerics.discrete_random;
with Ada.Strings.Unbounded;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
use Ada.Strings.unbounded;
use Ada.Text_IO;

package graph is
	n : Integer;
	d : Integer;
	h : Integer;
	max_delay : Integer;

	package intVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Natural);

	type pack(id: Integer) is record
		nextHop : intVector.Vector;
		cost: intVector.Vector;
		valid : intVector.Vector;
	end record;
	type package_access is access pack;

	type standard_pack(send_router, send_host, recv_router, recv_host: Integer) is record
		visited : intVector.Vector;
	end record;
	type stdPack_access is access standard_pack;
	package packVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => stdPack_access);

	package Pack_Queue_Interfaces is
        new Ada.Containers.Synchronized_Queue_Interfaces
            (Element_Type => stdPack_access);
    package Pack_Item_Queues is
        new Ada.Containers.Unbounded_Synchronized_Queues
            (Queue_Interfaces => Pack_Queue_Interfaces);

	protected type routingTable is
		procedure init(index : in Integer; size : in Integer);
		procedure preparePack(result : out package_access);
		procedure changeTable(myPack : in package_access);
		procedure print;
		procedure getNext(dest : in Integer; result : out Integer);
	private
		id : Integer;
        nextHop : intVector.Vector;
		cost: intVector.Vector;
		changed : intVector.Vector;
    end routingTable;

	task type host(id, router_id: Integer) is
		entry start;
		entry Receive(item: in stdPack_access);
		entry stop;
	end host;
	type host_access is access Host;
	package hostVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => host_access);

	type vertice(id: Integer) is record
	   next : intVector.Vector;
	   R : routingTable;
	   hosts : hostVector.Vector;
	   Queue : Pack_Item_Queues.Queue;
	end record;
	type vertice_access is access vertice;

	task type Sender(id : Integer) is 
		entry stop;
	end;
	type send_ptr is access Sender;

	task type Receiver(id : Integer) is
		entry Receive(item : in package_access);
	end Receiver;
	type rec_ptr is access Receiver;

	task type ForwReceive(id : Integer) is
		entry Receive(item : in stdPack_access);
	end ForwReceive;
	type forwRec_ptr is access ForwReceive;

	task type ForwPass(id : Integer);
	type forwPass_ptr is access ForwPass;

	task type Finish is
		entry change;
	end Finish;	
	type finish_ptr is access Finish;

	task Printer is
		entry print(text : String);
		entry println(text : String);
		entry newline;
		entry stop;
	end Printer;

	package verticeVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => vertice_access);
	package packageVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => package_access);
	package recVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => rec_ptr);
	package sendVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => send_ptr);
	package forwVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => forwRec_ptr);
	package forwVector2 is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => forwPass_ptr);

	type randRange is new Integer range 1..5000;
   	package randInt is new ada.numerics.discrete_random(randRange);
   	use randInt;
   	gen : Generator;

	verts : verticeVector.Vector;
	packs : packageVector.Vector;
	recs : recVector.Vector;
	sends : sendVector.Vector; 
	forws : forwVector.Vector;
	forws2 : forwVector2.Vector; 
	myFinish : Finish;

	procedure setConsts(n_const, d_const, h_const, max_delay_const: Integer);
	procedure graphGenerator;
	procedure printGraph;
	procedure startHosts;

end graph;