accessing
alreadySearchedSelectedItemsListMaxSize: anInteger

	[self alreadySearchedSelectedItemsList size > anInteger]
		whileTrue: [self alreadySearchedSelectedItemsList removeLast]