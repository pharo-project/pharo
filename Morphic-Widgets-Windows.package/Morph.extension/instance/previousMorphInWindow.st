*Morphic-Widgets-Windows
previousMorphInWindow
	"Answer the next morph in the window. Traverse
	from the receiver to its previous sibling's last submorph (recursive)
	or owner's previous sibling's last submorph (recursive) etc."

	^self submorphBefore notNil
		ifTrue: [self submorphBefore lastSubmorphRecursive]
		ifFalse: [self owner]