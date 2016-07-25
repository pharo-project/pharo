actions
expandBy: anInteger
	| selectionIndex |
	
	selectionIndex := self list selectedIndex.
	self list items: (self filteredStackOfSize: anInteger + self list listSize).
	self list setSelectedIndex: selectionIndex
	