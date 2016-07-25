A HistoryNode is composite node of an history tree. It is made to contain some other HistoryNode or HistoryLeaf instances.
A subnode is added with #addItem:
In order to add and feed a new subtree, one can use openGroup which add a new HistoryNode. When openGroup is sent to an HistoryNode named H, then a new group G is added and all subsequent sent of #addItem: or of #openGroup to H will update the new node G until G is closed by a closeGroup. 

As examples:
---------------
H := HistoryNode new.
H addItem: (i1 := HistoryLeaf new).
---------------
gives:
H
	i1

---------------
H := HistoryNode new.
H openGroup. "add a new group named g1"
H addItem: (i1 := HistoryLeaf new).
H addItem: (i2 := HistoryLeaf new).
--------------
gives:
H
	g1
		i1
		i2

--------------
H := HistoryNode new.
H openGroup. "add a new group named g1"
H openGroup. "add a new group named g2"
H addItem: (i1 := HistoryLeaf new).
H addItem: (i2 := HistoryLeaf new).
--------------
gives:
H
	g1
		g2
			i1
			i2
			
--------------
H := HistoryNode new.
H openGroup. "add a new group named g1"
H openGroup. "add a new group named g2"
H addItem: (i1 := HistoryLeaf new).
H closeGroup. "close g2"
H addItem: (i2 := HistoryLeaf new).
H closeGroup. "close g1"
H addItem: (i3 := HistoryLeaf new).
--------------
gives:
H
	g1
		g2
			i1
		i2
	i3
	
Also se HistoryNodeTest.


Instance Variables
	history:		<OrderedCollection>
	opened:		<Boolean>

history
	- The list of subnodes (HistoryLeaf or HistoryNode instances)

opened
	- true if the node is opened for adding
