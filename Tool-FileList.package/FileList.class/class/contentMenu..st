menu
contentMenu: aBuilder
	<contextMenu>
	<fileListContentMenu>
	
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		icon: (self iconNamed: #smallFindIcon).
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		icon: (self iconNamed: #smallFindIcon).
	(aBuilder item: #'Set search string' translated)
		keyText: 'h';
		selector: #setSearchString;
		withSeparatorAfter.
		
	(aBuilder item: #'Do again' translated)
		keyText: 'j';
		selector: #again;
		icon: (self iconNamed: #smallRedoIcon).
	(aBuilder item: #'Undo' translated)
		keyText: 'z';
		selector: #undo;
		icon: (self iconNamed: #smallUndoIcon);
		withSeparatorAfter.
		
	(aBuilder item: #'Copy' translated)
		keyText: 'c';
		selector: #copySelection;
		icon: (self iconNamed: #smallCopyIcon).
	(aBuilder item: #'Cut' translated)
		keyText: 'x';
		selector: #cut;
		icon: (self iconNamed: #smallCutIcon).
	(aBuilder item: #'Paste' translated)
		keyText: 'v';
		selector: #paste;
		icon: (self iconNamed: #smallPasteIcon).
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		icon: (self iconNamed: #smallPasteIcon);
		withSeparatorAfter .
		
	(aBuilder item: #'Do it' translated)
		keyText: 'd';
		selector: #doIt;
		icon: (self iconNamed: #smallDoItIcon).
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		icon: (self iconNamed: #smallPrintItIcon).
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		icon: (self iconNamed: #smallInspectItIcon).
	(aBuilder item: #'FileIn selection' translated)
		keyText: 'G';
		selector: #fileItIn;
		withSeparatorAfter.
		
	(aBuilder item: #'Accept' translated)
		keyText: 's';
		selector: #accept;
		icon: (self iconNamed: #smallOkIcon).
	(aBuilder item: #'Cancel' translated)
		keyText: 'l';
		selector: #cancel;
		icon: (self iconNamed: #smallCancelIcon);
		withSeparatorAfter.
		
	(aBuilder item: #'More...' translated)
		selector: #shiftedYellowButtonActivity.
