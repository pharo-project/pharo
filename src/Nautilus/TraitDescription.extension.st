Extension { #name : #TraitDescription }

{ #category : #'*Nautilus' }
TraitDescription >> buildAnalyzeSubMenu: aBuilder [

	| target |
	target := aBuilder model.
	
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
	
	
]
