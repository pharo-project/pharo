morphic ui
workingCopySearchField
	^ SearchMorph new
		model: self;
		setIndexSelector: #workingCopySearchAccept:; 
		searchList: self class workingCopySearchList;
		updateSelector: #workingCopySearchAccept:;
		yourself
