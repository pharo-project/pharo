accessing
alreadySearchedSelectedItemsListMaxSize: anObject
	anObject ifNil: [^self].
	alreadySearchedSelectedItemsListMaxSize := anObject.
	self allInstancesDo: [:each | each alreadySearchedSelectedItemsListMaxSize: anObject]