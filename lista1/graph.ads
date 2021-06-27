with Ada.Text_IO;
with Ada.Containers.Vectors;
with ada.numerics.discrete_random;
use Ada.Text_IO;

package graph is
	n : Integer;
	d : Integer;
	k : Integer;
	max_delay : Integer;

	package intVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Natural);

	type vertice(id: Integer) is record
	   next : intVector.Vector;
	   packages : intVector.Vector;
	end record;
	type vertice_access is access vertice;

	type pack(id: Integer) is record
		visited : intVector.Vector;
	end record;
	type package_access is access pack;

	task type Sender;

	task type Forwarder (vert: vertice_access) is
		entry Receive(item : in package_access);
	end Forwarder;
	type fwd_access is access Forwarder;

	task type Receiver is
		entry Receive(item : in package_access);
	end Receiver;

	task Printer is
		entry print(text : String);
		entry println(text : String);
		entry newline;
	end Printer;

	package verticeVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => vertice_access);
	package packageVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => package_access);
	package fwdVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => fwd_access);

	type randRange is new Integer range 1..5000;
   	package randInt is new ada.numerics.discrete_random(randRange);
   	use randInt;
   	gen : Generator;

	verts : verticeVector.Vector;
	packs : packageVector.Vector;
	fwds : fwdVector.Vector;
	myReceiver : Receiver; 

	procedure setConsts(n_const, d_const, k_const, max_delay_const : Integer);
	procedure graphGenerator;
	procedure printGraph;

end graph;