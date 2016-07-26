position
unoccupiedPositionBottomLeft

	| startPos area rect morphs |
	
	startPos := 10 negated @ (self height + 10).
	area := World clearArea.
	rect := (area bottomLeft - startPos) extent: self extent.
	
	morphs := self activeGrowlMorphs.
	[self is: rect saneWithRespectTo: morphs] whileFalse: [
		rect := rect translateBy: 0@(-10).
		rect top < 0 ifTrue: [^ area bottomLeft - startPos]
	].
	^ rect origin