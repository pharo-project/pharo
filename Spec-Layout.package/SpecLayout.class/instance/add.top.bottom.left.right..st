commands-advanced
add: aBlock top: top bottom: bottom left: left right: right
	| layout |
	
	layout := self computeLayoutFromTop: top bottom: bottom left: left right: right.
		
	self 
		add: aBlock
		origin: layout first
		corner: layout second
		offsetOrigin: layout third
		offsetCorner: layout fourth