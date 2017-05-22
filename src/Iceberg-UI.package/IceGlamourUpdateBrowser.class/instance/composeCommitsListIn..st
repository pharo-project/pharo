building
composeCommitsListIn: composite
	composite fastTable
		title: 'New commits to be imported';
		display: #incomingCommits;
		column: 'User' evaluated: #username width: 120;
		column: 'SHA' evaluated: #shortId width: 60;
		column: 'Date & time' evaluated: [:commit | commit datetime asLocalStringYMDHM ] width: 100;
		column: 'Status' evaluated: #status width: 80;
		column: 'Comment' evaluated: #comment;
		updateOn: IcePackageLoaded from: #announcer;
		addAction: self fetchAction;
		addAction: self pullAction;
		addSelectionAction: self loadAction;
		addSelectionAction: self mergeAction