requirements
subCollectionNotIn
" return a collection for which at least one element is not included in 'moreThan4Elements' "
	^ collectionNotIncluded 
		ifNil: [ collectionNotIncluded := (FloatArray new: 2) at:1 put: elementNotIn ; at: 2 put: elementNotIn  ; yourself ].