with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with ada.numerics.discrete_random;
with Ada.Strings.Unbounded;
use Ada.Strings.unbounded;
use Ada.Text_IO;

package graph is
	n : Integer;
	d : Integer;
	max_delay : Integer;

	package intVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Natural);

	type pack(id: Integer) is record
		nextHop : intVector.Vector;
		cost: intVector.Vector;
		valid : intVector.Vector;
	end record;
	type package_access is access pack;

	protected type routingTable is
		procedure init(index : in Integer; size : in Integer);
		procedure preparePack(result : out package_access);
		procedure changeTable(myPack : in package_access);
		procedure print;
	private
		id : Integer;
        nextHop : intVector.Vector;
		cost: intVector.Vector;
		changed : intVector.Vector;
    end routingTable;

	type vertice(id: Integer) is record
	   next : intVector.Vector;
	   R : routingTable;
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

	task type Finish is
		entry change;
	end Finish;	
	type finish_ptr is access Finish;

	task Printer is
		entry print(text : String);
		entry println(text : String);
		entry newline;
	end Printer;

	package verticeVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => vertice_access);
	package packageVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => package_access);
	package recVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => rec_ptr);
	package sendVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => send_ptr);

	type randRange is new Integer range 1..5000;
   	package randInt is new ada.numerics.discrete_random(randRange);
   	use randInt;
   	gen : Generator;

	verts : verticeVector.Vector;
	packs : packageVector.Vector;
	recs : recVector.Vector;
	sends : sendVector.Vector; 
	myFinish : Finish;

	procedure setConsts(n_const, d_const, max_delay_const: Integer);
	procedure graphGenerator;
	procedure printGraph;

end graph;