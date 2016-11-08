menu
menuOn: aBuilder
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubPlainTextMenu'
		model: RubEditingArea new) menu popUpInWorld" 

	<contextMenu>
	<RubPlainTextMenu>
	
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		iconName: #smallFindIcon.
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		iconName: #smallFindIcon.
	(aBuilder item: #'Set search string' translated)
		keyText: 'h';
		selector: #setSearchString.
	aBuilder model canChangeText
		ifTrue: [ 
			(aBuilder item: #'Do again' translated)
				keyText: 'j';
				selector: #again;
				iconName: #smallRedoIcon.
			(aBuilder item: #Undo translated)
				keyText: 'z';
				selector: #undo;
				iconName: #smallUndoIcon;
				withSeparatorAfter.
			self fontMenuOn: aBuilder ].
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		iconName: #smallCopyIcon.
	aBuilder model canChangeText
		ifTrue: [ 
			(aBuilder item: #Cut translated)
				keyText: 'x';
				selector: #cut;
				iconName: #smallCutIcon.
			(aBuilder item: #Paste translated)
				keyText: 'v';
				selector: #paste;
				iconName: #smallPasteIcon.
			(aBuilder item: #'Paste...' translated)
				selector: #pasteRecent;
				iconName: #smallCopyIcon;
				withSeparatorAfter.
			(aBuilder model model notNil and: [ aBuilder model acceptAllowed ])
				ifTrue: [ 
					(aBuilder item: #Accept translated)
						keyText: 's';
						selector: #accept;
						iconName: #smallOkIcon.
					(aBuilder item: #Cancel translated)
						keyText: 'l';
						selector: #cancel;
						iconName: #smallCancelIcon ] ]