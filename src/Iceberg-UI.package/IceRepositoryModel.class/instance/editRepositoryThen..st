actions
editRepositoryThen: aBlock
	(IceEditRepositoryModel repository: self repository) 
		onAccept: aBlock;
		openWithSpec