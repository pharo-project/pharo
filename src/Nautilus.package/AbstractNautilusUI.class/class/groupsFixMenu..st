menu
groupsFixMenu: aBuilder
	<contextMenu>
	<nautilusGlobalGroupFixMenu>
	| target |
	target := aBuilder model.
	(aBuilder item: #'Find Class...')
		keyText: 'f, c' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [target findClass];
		order: 0;
		help: 'Search for a class by name'.

	(aBuilder item: #'New group...')
		keyText: 'n, g' if: Nautilus useOldStyleKeys not;
		keyText: 'n' if: Nautilus useOldStyleKeys;
		action: [target addNewGroup];
		order: 100;
		withSeparatorAfter.
	
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [target fullBrowse];
		order: 999.