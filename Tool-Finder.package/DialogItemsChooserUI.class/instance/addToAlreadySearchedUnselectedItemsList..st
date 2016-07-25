accessing
addToAlreadySearchedUnselectedItemsList: aString
	self alreadySearchedUnselectedItemsList size = self alreadySearchedUnselectedItemsListMaxSize
		ifTrue: [self alreadySearchedUnselectedItemsList removeLast ].
	self alreadySearchedUnselectedItemsList addFirst: aString