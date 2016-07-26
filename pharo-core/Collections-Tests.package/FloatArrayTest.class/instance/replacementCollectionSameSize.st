requirements
replacementCollectionSameSize
" return a collection of size (secondIndex - firstIndex + 1)"
	^replacementCollectionSameSize 
		ifNil: [ 	replacementCollectionSameSize := FloatArray new: (self secondIndex  - self firstIndex  + 1).
				 1 to: replacementCollectionSameSize size do:
					[ :i | replacementCollectionSameSize at:i put: elementInNonEmpty ].
				replacementCollectionSameSize.
				 ].