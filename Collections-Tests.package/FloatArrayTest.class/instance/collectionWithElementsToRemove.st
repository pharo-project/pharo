requirements
collectionWithElementsToRemove
" return a collection of elements included in 'nonEmpty'  "
	^ nonEmptySubcollection 
	ifNil: [ nonEmptySubcollection := (FloatArray new:2 ) at:1 put: self nonEmpty first ; at:2 put: self nonEmpty last ; yourself ]