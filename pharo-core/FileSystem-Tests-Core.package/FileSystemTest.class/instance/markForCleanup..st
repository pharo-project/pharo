initialize-release
markForCleanup: anObject
	toDelete add: (filesystem resolve: anObject)