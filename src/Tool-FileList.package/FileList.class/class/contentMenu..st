menu
contentMenu: aBuilder
	<contextMenu>
	<fileListContentMenu>
	
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
		selector: #setSearchString;
		withSeparatorAfter.
		
	(aBuilder item: #'Do again' translated)
		keyText: 'j';
		selector: #again;
		iconName: #smallRedoIcon.
	(aBuilder item: #'Undo' translated)
		keyText: 'z';
		selector: #undo;
		iconName: #smallUndoIcon;
		withSeparatorAfter.
		
	(aBuilder item: #'Copy' translated)
		keyText: 'c';
		selector: #copySelection;
		iconName: #smallCopyIcon.
	(aBuilder item: #'Cut' translated)
		keyText: 'x';
		selector: #cut;
		iconName: #smallCutIcon.
	(aBuilder item: #'Paste' translated)
		keyText: 'v';
		selector: #paste;
		iconName: #smallPasteIcon.
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		iconName: #smallPasteIcon;
		withSeparatorAfter .
		
	(aBuilder item: #'Do it' translated)
		keyText: 'd';
		selector: #doIt;
		iconName: #smallDoItIcon.
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		iconName: #smallPrintItIcon.
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'FileIn selection' translated)
		keyText: 'G';
		selector: #fileItIn;
		withSeparatorAfter.
		
	(aBuilder item: #'Accept' translated)
		keyText: 's';
		selector: #accept;
		iconName: #smallOkIcon.
	(aBuilder item: #'Cancel' translated)
		keyText: 'l';
		selector: #cancel;
		iconName: #smallCancelIcon;
		withSeparatorAfter.
