morphic ui
repositorySearchField
	^ SearchMorph new
		model: self;
		setIndexSelector: #repositorySearchAccept:; 
		updateSelector: #repositorySearchAccept:;
		searchList: self class repositorySearchList;
		yourself