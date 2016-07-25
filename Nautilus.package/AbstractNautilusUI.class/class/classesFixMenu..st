menu
classesFixMenu: aBuilder
	<contextMenu>
	<nautilusGlobalClassFixMenu>
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Find Method...')
		keyText: 'f, m' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [target findMethod];
		order: 100;
		help: 'Search for a method by selector'.
		
	(aBuilder item: #'Find Class...')
		keyText: 'f, c' if: Nautilus useOldStyleKeys not;
		keyText: 'F' if: Nautilus useOldStyleKeys;
		action: [ target findClass ];
		order: 200;
		help: 'Search for a method by selector'.
	
	(aBuilder item: #'Add Class...')
		keyText: 'n, c' if: Nautilus useOldStyleKeys not;
		keyText: 'n' if: Nautilus useOldStyleKeys;
		action: [ target addClass ];
		order: 400;
		help: 'Create a new class'.
	(aBuilder item: #'Add Trait...')
		action: [ target addTrait ];
		order: 500;
		withSeparatorAfter.
		
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [target fullBrowse];
		order: 999;
		help: 'Open the same browser'.