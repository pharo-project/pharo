position
unoccupiedPositionTopLeft

	| startPos area rect morphs |
	
	startPos := 10@10.
	area := World clearArea.
	rect := area topLeft + (startPos) extent: self extent.
	
	morphs := self activeGrowlMorphs.
	[self is: rect saneWithRespectTo: morphs] whileFalse: [
		rect := rect translateBy: 0@10.
		rect bottom > area height ifTrue: [^ area topLeft + startPos ]
	].
	^ rect origin