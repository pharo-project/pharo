selection change
shiftSelectMorph: aNodeMorph 
	| m |
	m := aNodeMorph ifNil: [self allNodeMorphs last].
	self emptySelection.
	self searchedElement: nil.
	self 
		from: firstClickedMorph 
		to: m 
		do: [:nd | self addToSelection: nd]. 
	self lastClickedMorph: m.
	^ m path collect: [:p | p complexContents]