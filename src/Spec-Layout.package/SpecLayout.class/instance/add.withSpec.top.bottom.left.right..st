commands-advanced
add: aBlock withSpec: aSpec top: top bottom: bottom left: left right: right
	| layout |
	
	layout := self computeLayoutFromTop: top bottom: bottom left: left right: right.
		
	self 
		add: aBlock
		withSpec: aSpec 
		origin: layout first
		corner: layout second
		offsetOrigin: layout third
		offsetCorner: layout fourth