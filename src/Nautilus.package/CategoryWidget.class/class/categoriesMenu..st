menus
categoriesMenu: aBuilder
	<nautilusGlobalProtocolMenu>
	| target |
	
	target := aBuilder model.
	
	(aBuilder item: #'Find Method...')
		keyText: 'f, m' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [ target findMethod ];
		order: 0;
		help: 'Search for a method by name'.
			
	(aBuilder item: #'Add protocol...')
		keyText: 'n, t' if: Nautilus useOldStyleKeys not;
		action: [ target addCategory ];
		order: 100;
		withSeparatorAfter.
	
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [ target fullBrowse ];
		order: 200.
			
	(aBuilder item: #'Browse External Package')
		enabledBlock: [ target enableCategorySingleSelection and: [ target selectedCategories anySatisfy: [ :cat | cat beginsWith: '*' ] 	] ];
		action: [ (RPackage organizer packageForProtocol: target selectedCategory inClass: target class) browse ];
		order: 250;
		withSeparatorAfter.
		
	(aBuilder item: #'Categorize all uncategorized and remove empty')
		keyText: 'h, C' if: Nautilus useOldStyleKeys not;
		action: [ target categorizeAllUncategorizedMethods ];
		order: 1100.	
		
	(aBuilder item: #'Remove empty protocols')
		action: [ target removeEmptyCategories ];
		order: 1200;
		withSeparatorAfter.
		
	target selectedCategory ifNil: [ ^ target ].
	
	(aBuilder item: #'Rename...')
		keyText: 'r, t' if: Nautilus useOldStyleKeys not;
		keyText: 'r' if: Nautilus useOldStyleKeys;
		action: [ target renameCategory ];
		order: 1300;
		enabledBlock: [ target enableCategorySingleSelection ].	
		
	(aBuilder item: #'Remove...')
		keyText: 'x, t' if: Nautilus useOldStyleKeys not;
		keyText: 'x' if: Nautilus useOldStyleKeys;
		action: [ target removeCategories ];
		order: 1400;
		iconName: #removeIcon;
		withSeparatorAfter.
	
	(aBuilder item: #'File Out')
		action: [ target fileOutCategories ];
		order: 1600.