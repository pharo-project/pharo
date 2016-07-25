menu
menuOn: aBuilder
	"Specify the menu used when writing code."

	<contextMenu>
	<RubSmalltalkCodeMenu>
	| uitheme |
	uitheme := Smalltalk ui icons.
	(aBuilder item: #'Do it' translated)
		order: 1;
		keyText: 'd';
		selector: #doIt;
		help: nil;
		icon: (uitheme iconNamed: #smallDoItIcon).
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		help: nil;
		icon: (uitheme iconNamed: #smallPrintItIcon).
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		icon: (uitheme iconNamed: #smallInspectItIcon).
	(aBuilder item: #'Basic Inspect it' translated)
		keyText: 'I';
		selector: #basicInspectIt;
		help: nil;
		icon: (uitheme iconNamed: #smallInspectItIcon).
	(aBuilder item: #'Debug it' translated)
		keyText: 'D';
		selector: #debugIt;
		help: nil;
		icon: (uitheme iconNamed: #smallDebugIcon).
	(aBuilder item: #'Profile it' translated)
		selector: #tallyIt;
		icon: (uitheme iconNamed: #smallDebugIcon);
		withSeparatorAfter.
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		help: nil;
		icon: (uitheme iconNamed: #smallFindIcon).
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		help: nil;
		icon: (uitheme iconNamed: #smallFindIcon).
	self extendedSearchMenuOn: aBuilder.
	(aBuilder item: #'Do again' translated)
		keyText: 'j';
		selector: #again;
		help: nil;
		icon: (uitheme iconNamed: #smallRedoIcon).
	(aBuilder item: #Undo translated)
		keyText: 'z';
		selector: #undo;
		help: nil;
		icon: (uitheme iconNamed: #smallUndoIcon);
		withSeparatorAfter.
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		help: nil;
		icon: (uitheme iconNamed: #smallCopyIcon).
	(aBuilder item: #Cut translated)
		keyText: 'x';
		selector: #cut;
		help: nil;
		icon: (uitheme iconNamed: #smallCutIcon).
	(aBuilder item: #Paste translated)
		keyText: 'v';
		selector: #paste;
		help: nil;
		icon: (uitheme iconNamed: #smallPasteIcon).
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		help: nil;
		icon: (uitheme iconNamed: #smallCopyIcon);
		withSeparatorAfter.
	aBuilder model ifNotNil: [:mod | mod model
		ifNotNil: [ 
			(aBuilder item: #Accept translated)
				keyText: 's';
				selector: #accept;
				icon: (uitheme iconNamed: #smallOkIcon).
			(aBuilder item: #Cancel translated)
				keyText: 'l';
				selector: #cancel;
				icon: (uitheme iconNamed: #smallCancelIcon) ]]