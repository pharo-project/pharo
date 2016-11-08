menu
menuOn: aBuilder
	"Specify the menu used when writing code."

	<contextMenu>
	<RubSmalltalkCodeMenu>
	(aBuilder item: #'Do it' translated)
		order: 1;
		keyText: 'd';
		selector: #doIt;
		help: nil;
		iconName: #smallDoItIcon.
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		help: nil;
		iconName: #smallPrintItIcon.
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'Basic Inspect it' translated)
		keyText: 'I';
		selector: #basicInspectIt;
		help: nil;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'Debug it' translated)
		keyText: 'D';
		selector: #debugIt;
		help: nil;
		iconName: #smallDebugIcon.
	(aBuilder item: #'Profile it' translated)
		selector: #tallyIt;
		iconName: #smallDebugIcon;
		withSeparatorAfter.
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		help: nil;
		iconName: #smallFindIcon.
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		help: nil;
		iconName: #smallFindIcon.
	self extendedSearchMenuOn: aBuilder.
	(aBuilder item: #'Do again' translated)
		keyText: 'j';
		selector: #again;
		help: nil;
		iconName: #smallRedoIcon.
	(aBuilder item: #Undo translated)
		keyText: 'z';
		selector: #undo;
		help: nil;
		iconName: #smallUndoIcon;
		withSeparatorAfter.
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		help: nil;
		iconName: #smallCopyIcon.
	(aBuilder item: #Cut translated)
		keyText: 'x';
		selector: #cut;
		help: nil;
		iconName: #smallCutIcon.
	(aBuilder item: #Paste translated)
		keyText: 'v';
		selector: #paste;
		help: nil;
		iconName: #smallPasteIcon.
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		help: nil;
		iconName: #smallCopyIcon;
		withSeparatorAfter.
	aBuilder model
		ifNotNil: [ :mod | 
			mod model
				ifNotNil: [ (aBuilder item: #Accept translated)
						keyText: 's';
						selector: #accept;
						iconName: #smallOkIcon.
					(aBuilder item: #Cancel translated)
						keyText: 'l';
						selector: #cancel;
						iconName: #smallCancelIcon ] ]