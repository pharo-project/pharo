specs
buildDefaultSpec: col
	col
		newRow: [:row | row
			add: #remoteUrlLabel width: 100;
			add: #remoteUrl ]
		height: self inputTextHeight.
		
	super buildDefaultSpec: col