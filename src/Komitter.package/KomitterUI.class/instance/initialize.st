initialization
initialize

	model := nil asValueHolder.
	canCommit := false asValueHolder.
	
	super initialize.
	model
		whenChangedDo: [ :m | 
			[ self populateTreeFor: m ] fork.
			message text: m lastMessage.
			self populateRemotes ]