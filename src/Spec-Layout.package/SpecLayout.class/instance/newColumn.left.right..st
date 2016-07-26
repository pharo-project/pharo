commands-advanced
newColumn: aBlock left: left right: right
	| layout |
	
	layout := self computeLayoutFromTop: nil bottom: nil left: left right: right.

	self 
		newColumn: aBlock
		origin: layout first
		corner: layout second
		offsetOrigin: layout third
		offsetCorner: layout fourth