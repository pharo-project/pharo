items creation
buildSearchMorph
	^ searchMorph := SearchMorph new
			model: self;
			acceptSelector: #searchAccept:;
			updateSelector: #searchUpdate:;
			searchList: self class searchList;
			keystrokeSelector: #searchKeystroke:
			yourself.	
	