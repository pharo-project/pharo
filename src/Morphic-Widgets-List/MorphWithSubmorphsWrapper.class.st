"
Display a morph in a SimpleHierarchicalListMorph, and arrange to recursively display the morph's submorphs.  The ""item"" that is wrapped is the morph to display.
"
Class {
	#name : #MorphWithSubmorphsWrapper,
	#superclass : #ListItemWrapper,
	#category : 'Morphic-Widgets-List'
}

{ #category : #hierarchy }
MorphWithSubmorphsWrapper >> contents [
	^item submorphs collect: [ :m |
		self class with: m ]
]
