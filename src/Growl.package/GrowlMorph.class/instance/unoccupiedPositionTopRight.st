position
unoccupiedPositionTopRight

	| startPos area rect morphs |
	
	startPos := (self width + 10 @ 10 negated).
	area := World clearArea.
	rect := (area topRight - startPos) extent: self extent.
	
	morphs := self activeGrowlMorphs.	
	[self is: rect saneWithRespectTo: morphs] whileFalse: [
		rect := rect translateBy: 0@10.
		rect bottom > area height ifTrue: [^ (area topRight - startPos) ]
	].

	^ rect origin