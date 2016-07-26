accessing
addToAlreadySearchedSelectedItemsList: aString
	self alreadySearchedSelectedItemsList size = self alreadySearchedSelectedItemsListMaxSize
		ifTrue: [self alreadySearchedSelectedItemsList removeLast ].
	self alreadySearchedSelectedItemsList addFirst: aString