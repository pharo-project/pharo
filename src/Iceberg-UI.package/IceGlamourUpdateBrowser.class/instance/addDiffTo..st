building
addDiffTo: browser
	browser transmit 
		from: #commits; 
		from: #changes; 
		to: #diff; 
		andShow: [ :a | 
			a swappedColorDiff 
				title: 'Left: working copy / Right: incoming updates';
				display: [ :commitInfo :change | self buildDiffInput: change ]. 
			a morph 
				title: 'Commit info';
				display: [ :commitInfo | (IceCommitInfoModel for: commitInfo) buildWithSpec ] ]
	