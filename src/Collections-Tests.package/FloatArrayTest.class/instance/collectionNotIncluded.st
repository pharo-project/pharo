requirements
collectionNotIncluded
" return a collection for wich each element is not included in 'nonEmpty' "
	^ collectionNotIncluded 
		ifNil: [ collectionNotIncluded := (FloatArray new: 2) at:1 put: elementNotIn ; at: 2 put: elementNotIn  ; yourself ].