building
composeCommitsIn: composite
	composite fastTable
		title: 'Commits';
		display: #commits;
		column: 'Date & time' 
			evaluated: [:commit | 
				commit datetime asLocalStringYMDHM asText 
					in: [ :text | 
						commit isLoaded 
							ifTrue: [ text ] 
							ifFalse: [ text allBold ] ] ] 
			width: 120;
		column: 'Id' evaluated: #shortId width: 50;
		column: 'User' evaluated: #username width: 120;
		column: 'Parents' evaluated: [ :commit | ', ' join: (commit parents collect: #shortId) ] width: 100;
		column: 'Comment' evaluated: #comment width: 300;
		
		addSelectionAction: self inspectAction;
		addSelectionAction: self mergeIntoCurrentBranchAction;
		addSelectionAction: self mergeIntoAction.
