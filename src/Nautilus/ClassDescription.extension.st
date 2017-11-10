Extension { #name : #ClassDescription }

{ #category : #'*Nautilus' }
ClassDescription >> buildAnalyzeSubMenu: aBuilder [

	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Inst var references...')
		action: [target browseInstVarRefs];
		parent: #Analyze;
		order: 0.

	(aBuilder item: #'Inst var assignments...')
		action: [target browseInstVarDefs];
		parent: #Analyze;
		order: 100.
	
	(aBuilder item: #'Generate initialize method')
		keyText: 'h, i' if: Nautilus useOldStyleKeys not;
		keyText: 'I' if: Nautilus useOldStyleKeys;
		action: [ target generateInitialize ];
		parent: #Analyze;
		order: 350.
		
	(aBuilder item: #'Force the generation of the initialize method')
		keyText: 'h, k' if: Nautilus useOldStyleKeys not;
		keyText: 'K' if: Nautilus useOldStyleKeys;		
		action: [ target forceGenerateInitialize ];
		parent: #Analyze;
		order: 375;
		enabledBlock: [ target showInstance ];
		withSeparatorAfter.
		
	(aBuilder item: #'Class var refs...')
		action: [target browseClassVarRefs];
		parent: #Analyze;
		order: 400.
		
	(aBuilder item: #'Class refs...')
		keyText: 'b, N' if: Nautilus useOldStyleKeys not;
		keyText: 'N' if: Nautilus useOldStyleKeys;
		action: [target browseClassRefs];
		parent: #Analyze;
		order: 500.

	(target selectedClass inheritsFrom: SharedPool )
		ifTrue: [ 
			(aBuilder item: #'Shared Pool Users')
				action: [ target systemNavigation browseSharedPoolUsersOf: target selectedClass ];
				parent: #Analyze;
				enabledBlock: [ target enableSingleClassSelection ];
				order: 700].
				
	(aBuilder item: #'Unsent methods...')
		action: [target browseUnusedMethods.];
		parent: #Analyze;
		order: 800.
		
	(aBuilder item: #'Unreferenced inst vars...')
		action: [target showUnreferencedInstVars.];
		parent: #Analyze;
		order: 900.
		
	(aBuilder item: #'Unreferenced class vars...')
		action: [target showUnreferencedClassVars.];
		parent: #Analyze;
		order: 1000.
	
	
]
