ui
menuOn: aBuilder
	<HelpBrowserContextMenu>
	
	(aBuilder item: #'Do it' translated)
		keyText: 'd';
		selector: #doIt;
		help: nil;
		icon: (Smalltalk ui icons iconNamed: #smallDoIt).
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		help: nil;
		icon: (Smalltalk ui icons iconNamed: #smallPrintIt).
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		icon: (Smalltalk ui icons iconNamed: #smallInspectIt).
