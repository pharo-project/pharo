morphic ui
packageSearchField
	^ SearchMorph new
		model: self;
		setIndexSelector: #packageSearchAccept:;
		updateSelector: #packageSearchAccept:;
		searchList: self class packageSearchList;
		yourself