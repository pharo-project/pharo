menu
packagesFixMenu: aBuilder
	<contextMenu>
	<nautilusGlobalPackageTreeFixMenu>
	
	| target |
	
	target := aBuilder model.
	
	(aBuilder item: #'Find Class...')
		keyText: 'f, c' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [target findClass];
		order: 0;
		help: 'Search for a class by name'.
		
	(aBuilder item: #'Find Package...')
		keyText: 'f, p' if: Nautilus useOldStyleKeys not;
		keyText: 'F' if: Nautilus useOldStyleKeys;
		action: [ target findPackage ];
		order: 100;
		help: 'Search for a package by name';
		withSeparatorAfter.
		
	(aBuilder item: #'Add package...')
		keyText: 'n, p' if: Nautilus useOldStyleKeys not;
		keyText: 'n' if: Nautilus useOldStyleKeys;
		action: [target addPackage];
		order: 200;
		help: 'Add a package'.
		
	(aBuilder item: #'Add tag...')
		action: [ target addClassCategory ];
		order: 210;
		help: 'Add a tag/category in package';
		withSeparatorAfter.
		
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [target fullBrowse];
		order: 999;
		help: 'Open the same browser'.