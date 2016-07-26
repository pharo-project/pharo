computation
sortedElements
	"Return the list of elements sorted appropriately."
	"Nodes are already sorted, we just return each's element."
	^ hierarchy collect: [ :each | each element ]