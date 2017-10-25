tests - transforming
testFlipByCenterAt

	| rectangle |
	
	rectangle := 10 @ 10 corner: 20 @ 20.
	self assert: (10 @ 0 corner: 20 @ 10) equals: (rectangle flipBy: #vertical centerAt: 10 @ 10).
	self assert: (0 @ 10 corner: 10 @ 20) equals: (rectangle flipBy: #horizontal centerAt: 10 @ 10)