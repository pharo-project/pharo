initialization
initialize
	super initialize.
	
	rowSize := 25.
	columnSize := rowSize // 2.
	nodeBuilder := HiNodeBuilder new.
	linkBuilders := OrderedCollection new.