menu
classesMenu: aBuilder
	<contextMenu>
	<nautilusGlobalClassMenu>
	| target |
	target := aBuilder model.
	target selectedClass ifNil: [ ^ target ].
	(aBuilder item: #'Rename...')
		keyText: 'r,c' if: Nautilus useOldStyleKeys not;
		keyText: 'r' if: Nautilus useOldStyleKeys;
		action: [ target renameClass ];
		enabledBlock: [ target selectedClasses size <= 1 ];
		order: -99;
		withSeparatorAfter.
		
	(aBuilder item: #'Class refs...')
		keyText: 'b, N' if: Nautilus useOldStyleKeys not;
		keyText: 'N' if: Nautilus useOldStyleKeys;
		action: [target browseClassRefs];
		order: -99.	
		
	(aBuilder item: #'Browse scoped')
		keyText: 'b, s' if: Nautilus useOldStyleKeys not;
		keyText: 'B' if: Nautilus useOldStyleKeys;
		action: [ target restrictedBrowseClass ];
		order: 1000;
		help: 'Open a browser on a restricted view'.
	target selectedClass isTrait
		ifTrue: [ 
			(aBuilder item: #'Browse users')
				action: [ target restrictedBrowseTraitUsers ];
				order: 1050 ].
	(target selectedClasses allSatisfy: [ :each | each isTestCase and: [ each isAbstract not ] ])
		ifTrue: [ 
			(aBuilder item: #'Run tests')
				keyText: 'j, c' if: Nautilus useOldStyleKeys not;
				keyText: 't' if: Nautilus useOldStyleKeys;
				action: [ target runClassTests ];
				order: 1200 ].
	(aBuilder item: #Analyze)
		enabledBlock: [ target selectedClasses size <= 1 ];
		order: 0;
		withSeparatorAfter.
	(aBuilder item: #'Jump to test class')
		keyText: 'g, j' if: Nautilus useOldStyleKeys not;
		keyText: 'j' if: Nautilus useOldStyleKeys;
		action: [ target createTestForSelectedClass ];
		enabledBlock: [ target selectedClasses size = 1 ];
		order: 300;
		withSeparatorAfter.
	(aBuilder item: #'Move to package...')
		keyText: 'm, c' if: Nautilus useOldStyleKeys not;
		action: [ target moveInPackage ];
		order: 1400.
	(aBuilder item: #'Copy...')
		keyText: 'c, c' if: Nautilus useOldStyleKeys not;
		keyText: 'c' if: Nautilus useOldStyleKeys;
		action: [ target copyClasses ];
		order: 1600.
	(aBuilder item: #'Remove...')
		keyText: 'x, c' if: Nautilus useOldStyleKeys not;
		keyText: 'x' if: Nautilus useOldStyleKeys;
		action: [ target removeClassWithRefactoringEngine: target selectedClasses asOrderedCollection ];
		icon: (Smalltalk ui icons iconNamed: #removeIcon);
		order: 1700;
		withSeparatorAfter.
	(aBuilder item: #'Inspect Methods')
		action: [ target inspectMethods ];
		withSeparatorAfter;
		order: 1750.
	(aBuilder item: #'File Out')
		action: [ target fileOutClass ];
		withSeparatorAfter;
		order: 1800