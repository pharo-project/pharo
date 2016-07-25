menu
changeSetMenu1: aBuilder
	<contextMenu>
	<changeSorterChangeSetList1Menu>
	
	| target |
	target := aBuilder model.
	
	target selectedChangeSet ifNotNil: [
		(aBuilder item: #'Make changes go to me')
			keyText: 'm';
			selector: #setCurrentChangeSet.
		(aBuilder item: #'New change set...')
			keyText: 'n';
			selector: #newSet ].
	
	(aBuilder item: #'Find...')
		keyText: 'f';
		selector: #findChangeSet.

	target selectedChangeSet ifNotNil: [	
		aBuilder withSeparatorAfter.
		(aBuilder item: #'Rename change set')
			keyText: 'r';
			selector: #rename.
		(aBuilder item: #'File out')
			keyText: 'o';
			selector: #fileOut.
		(aBuilder item: #'Browse methods')
			keyText: 'b';
			selector: #browseChangeSet.
		(aBuilder item: #'Browse change set')
			keyText: 'B';
			selector: #openChangeSetBrowser ].
		