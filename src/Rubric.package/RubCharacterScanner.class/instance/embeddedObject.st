scanning
embeddedObject
	| savedIndex |
	savedIndex := lastIndex.
	text
		attributesAt: lastIndex
		do: [ :attr | 
			attr anchoredMorph
				ifNotNil: [ 
					"Following may look strange but logic gets reversed.
			If the morph fits on this line we're not done (return false for true) 
			and if the morph won't fit we're done (return true for false)"
					(self placeEmbeddedObject: attr anchoredMorph)
						ifFalse: [ ^ true ] ] ].
	lastIndex := savedIndex + 1.	"for multiple(!) embedded morphs"
	^ false