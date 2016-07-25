*Polymorph-Widgets
nextMorphAcrossInWindow
	"Answer the next morph in the window. Traverse
	from the receiver to its next sibling or owner's next sibling etc.
	Make sure we behave like a normal Morph, if we aren't a toplevel window ( or not visible) "

	^ (self owner notNil and: [ self owner isWorldMorph ])
		ifTrue: [ self ]
		ifFalse: [ super nextMorphAcrossInWindow ]