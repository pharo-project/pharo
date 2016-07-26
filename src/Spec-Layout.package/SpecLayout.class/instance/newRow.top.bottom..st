commands-advanced
newRow: aBlock top: top bottom: bottom
	| layout |
	
	layout := self computeLayoutFromTop: top bottom: bottom left: nil right: nil.
		
	self 
		newRow: aBlock
		origin: layout first
		corner: layout second
		offsetOrigin: layout third
		offsetCorner: layout fourth