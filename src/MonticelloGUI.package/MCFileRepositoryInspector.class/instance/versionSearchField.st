morphic ui
versionSearchField
	^ SearchMorph new
		model: self;
		setIndexSelector: #versionSearchAccept:;
		updateSelector: #versionSearchAccept:;
		searchList: self class versionSearchList;
		yourself