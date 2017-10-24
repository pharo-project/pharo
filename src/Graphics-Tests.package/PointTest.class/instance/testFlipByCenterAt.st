tests - point functions
testFlipByCenterAt

	| center a direction |
	
	center := 4 @ 5.
	
	a := 0 @ 0.
	direction := #vertical.
	self assert: (a flipBy: direction centerAt: center) = (0 @ 10).
	direction := #horizontal.
	self assert: (a flipBy: direction centerAt: center) = (8 @ 0).
	direction := #diagonal.
	self should: [ a flipBy: direction centerAt: center ] raise: Error description: 'unrecognizable direction'.
	
	a := 1 @ 2.
	direction := #vertical.
	self deny: (a flipBy: direction centerAt: center) = (0 @ 10).
	direction := #horizontal.
	self deny: (a flipBy: direction centerAt: center) = (8 @ 0).
	direction := #diagonal.
	self should: [ a flipBy: direction centerAt: center ] raise: Error description: 'unrecognizable direction'