*Morphic-Widgets-Windows
addPaneVSplitterBetween: leftMorph and: rightMorphs 

	| targetX minY maxY splitter |
	targetX := leftMorph layoutFrame rightFraction.
	minY := (rightMorphs detectMin: [:each | each layoutFrame topFraction]) layoutFrame topFraction.
	maxY := (rightMorphs detectMax: [:each | each layoutFrame bottomFraction]) layoutFrame bottomFraction.
	
	splitter := ProportionalSplitterMorph new.
	splitter layoutFrame: ( (targetX @ minY corner: targetX @ maxY) asLayoutFrame 
		leftOffset: leftMorph layoutFrame rightOffset;
		rightOffset: 4+ leftMorph layoutFrame rightOffset;
		topOffset:  leftMorph layoutFrame topOffset;
		bottomOffset: leftMorph layoutFrame bottomOffset).

	self addMorphBack: (splitter position: self position).