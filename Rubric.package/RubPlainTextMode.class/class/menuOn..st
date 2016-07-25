menu
menuOn: aBuilder
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubPlainTextMenu'
		model: RubEditingArea new) menu popUpInWorld" 

	<contextMenu>
	<RubPlainTextMenu>
	| uitheme |
	uitheme := Smalltalk ui icons.
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		icon: (uitheme iconNamed: #smallFindIcon).
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		icon: (uitheme iconNamed: #smallFindIcon).
	(aBuilder item: #'Set search string' translated)
		keyText: 'h';
		selector: #setSearchString.
	aBuilder model canChangeText
		ifTrue: [ 
			(aBuilder item: #'Do again' translated)
				keyText: 'j';
				selector: #again;
				icon: (uitheme iconNamed: #smallRedoIcon).
			(aBuilder item: #Undo translated)
				keyText: 'z';
				selector: #undo;
				icon: (uitheme iconNamed: #smallUndoIcon);
				withSeparatorAfter.
			self fontMenuOn: aBuilder ].
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		icon: (uitheme iconNamed: #smallCopyIcon).
	aBuilder model canChangeText
		ifTrue: [ 
			(aBuilder item: #Cut translated)
				keyText: 'x';
				selector: #cut;
				icon: (uitheme iconNamed: #smallCutIcon).
			(aBuilder item: #Paste translated)
				keyText: 'v';
				selector: #paste;
				icon: (uitheme iconNamed: #smallPasteIcon).
			(aBuilder item: #'Paste...' translated)
				selector: #pasteRecent;
				icon: (uitheme iconNamed: #smallCopyIcon);
				withSeparatorAfter.
			(aBuilder model model notNil and: [ aBuilder model acceptAllowed ])
				ifTrue: [ 
					(aBuilder item: #Accept translated)
						keyText: 's';
						selector: #accept;
						icon: (uitheme iconNamed: #smallOkIcon).
					(aBuilder item: #Cancel translated)
						keyText: 'l';
						selector: #cancel;
						icon: (uitheme iconNamed: #smallCancelIcon) ] ]