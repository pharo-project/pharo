building
composeBrowserOn: browser
	browser 
		row: [:row | 
			row 
				column: #commits; 
				column: #changes ]; 
		row: #diff.
	
	browser transmit to: #commits; andShow: [ :a | self composeCommitsIn: a ].

	self addChangesTreesTo: browser.
	self addDiffTo: browser.
	self addCommitInfoTo: browser