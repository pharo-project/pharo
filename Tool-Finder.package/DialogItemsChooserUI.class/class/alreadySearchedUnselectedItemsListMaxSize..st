accessing
alreadySearchedUnselectedItemsListMaxSize: anObject
	anObject ifNil: [^self].
	alreadySearchedUnselectedItemsListMaxSize := anObject.
	self allInstancesDo: [:each | each alreadySearchedUnselectedItemsListMaxSize: anObject]