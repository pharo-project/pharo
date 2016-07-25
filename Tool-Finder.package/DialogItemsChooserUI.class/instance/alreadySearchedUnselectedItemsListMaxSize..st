accessing
alreadySearchedUnselectedItemsListMaxSize: anInteger

	[self alreadySearchedUnselectedItemsList size > anInteger]
		whileTrue: [self alreadySearchedUnselectedItemsList removeLast]