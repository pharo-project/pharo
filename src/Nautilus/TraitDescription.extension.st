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

{ #category : #'*Nautilus' }
TraitDescription >> codeRewritingClass: aBuilder [
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Rewrite Code')
		action: [ target refactor rewriteCode ];
		parent: #'Code Rewriting';
		order: 0.

	(aBuilder item: #'Search Code')
		action: [ target refactor searchCode ];
		parent: #'Code Rewriting';
		order: 100.
		
	(aBuilder item: #'Type class')
		action: [ target refactor typeClass ];
		parent: #'Code Rewriting';
		order: 200;
		withSeparatorAfter.
		
	(aBuilder item: #'Category Regex')
		action: [ target refactor categoryRegex ];
		parent: #'Code Rewriting';
		order: 300.
		
	(aBuilder item: #'Class Regex')
		action: [ target refactor classRegex ];
		parent: #'Code Rewriting';
		order: 400.
		
	(aBuilder item: #'Protocol Regex')
		action: [ target refactor protocolRegex ];
		parent: #'Code Rewriting';
		order: 500.
		
	(aBuilder item: #'Source Regex')
		action: [ target refactor sourceRegex ];
		parent: #'Code Rewriting';
		order: 600;
		withSeparatorAfter.
]

{ #category : #'*Nautilus' }
TraitDescription >> groupRefactoring: aBuilder [
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Rewrite Code')
		order:  100;
		withSeparatorAfter.
	
	(aBuilder item: #'Rewrite Code')
		action: [ target refactor rewriteCode ];
		parent: #'Rewrite Code';
		order: 0.

	(aBuilder item: #'Search Code')
		action: [ target refactor searchCode ];
		parent: #'Rewrite Code';
		order: 100.
]

{ #category : #'*Nautilus' }
TraitDescription >> instVarRefactoring: aBuilder [
	
]

{ #category : #'*Nautilus' }
TraitDescription >> methodRefactoring: aBuilder [
	| target selectedMethod selectedMethods |

	target := aBuilder model.
	selectedMethod := target selectedMethod.
	selectedMethods := target selectedMethods.
	selectedMethod ifNil:[ ^ target ].
	
	(aBuilder item: #'Add a parameter')
		action: [ target refactor addAParameterFor: selectedMethod ];
		order: 50;
		parent: #'Refactoring'.
		
	(aBuilder item: #'Deprecate')
		action: [ target refactor deprecateMethodFor: selectedMethod ];
		order: 75;
		parent: #'Refactoring'.
			
	(aBuilder item: #'Inline parameter')
		action: [ target refactor inlineParameterFor: selectedMethod ];
		order: 100;
		parent: #'Refactoring'.
			
	(aBuilder item: #'Inline target sends')
		action: [ target refactor inlineAllSendersFor: selectedMethod ];
		order: 200;
		parent: #'Refactoring'.
			
	(aBuilder item: #'Move')
		action: [ target refactor moveMethodFor: selectedMethod ];
		order: 300;
		parent: #'Refactoring'.
			
	(aBuilder item: #'Remove')
		action: [ target refactor removeMethodsFor: selectedMethods ];
		order: 700;
		parent: #'Refactoring'.
		
	(aBuilder item: #'Remove parameter')
		action: [ target refactor removeParameterFor: selectedMethod ];
		order: 800;
		enabledBlock: [ selectedMethods size < 2 ];
		parent: #'Refactoring'.
			
	(self renameMethodAllItem: aBuilder)
		order: 850;
		parent: #'Refactoring';
		withSeparatorAfter.
				
	(aBuilder item: #'Undo')
		order: 900;
		label: target refactor undoLabel;
		action: [ target refactor undoOperation ];
		enabled: target refactor undoEnabled;
		parent: #'Refactoring'.
	
	(aBuilder item: #'Redo')
		order: 1000;
		label: target refactor redoLabel;
		action: [ target refactor redoOperation ];
		enabled: target refactor redoEnabled;
		parent: #'Refactoring';
		withSeparatorAfter.
]

{ #category : #'*Nautilus' }
TraitDescription >> packageRefactoringMenu: aBuilder [
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Rewrite Code')
		order:  100;
		withSeparatorAfter.
	
	(aBuilder item: #'Rewrite Code')
		action: [ target refactor rewriteCode ];
		parent: #'Rewrite Code';
		order: 0.

	(aBuilder item: #'Search Code')
		action: [ target refactor searchCode ];
		parent: #'Rewrite Code';
		order: 100.
]

{ #category : #'*Nautilus' }
TraitDescription >> refactoringMenu: aBuilder [
	| target |
	
	target := aBuilder model.
	
	(aBuilder item: #'Refactoring')
			order: -100.
	
	(aBuilder item: #'Code Rewriting')
			order: 0;
			parent: #'Refactoring'.
			
	(aBuilder item: #'Trait Refactoring')
			order: 100;
			parent: #'Refactoring';
			withSeparatorAfter.
		
	(aBuilder item: #'Undo')
			order: 400;
			label: target refactor undoLabel;
			action: [ target refactor undoOperation ];
			enabled: target refactor undoEnabled;
			parent: #'Refactoring'.
	
	(aBuilder item: #'Redo')
			order: 500;
			label: target refactor redoLabel;
			action: [ target refactor redoOperation ];
			enabled: target refactor redoEnabled;
			parent: #'Refactoring';
			withSeparatorAfter.
]

{ #category : #'*Nautilus' }
TraitDescription >> refactoringMethod: aBuilder [

	(aBuilder item: #'Refactoring')
		order: -100.
			
	(self renameMethodAllItem: aBuilder)
		order: -90;
		withSeparatorAfter
]

{ #category : #'*Nautilus' }
TraitDescription >> refactoringSubmenu: aBuilder [
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Rename...')
		keyText: 'r, c' if: Nautilus useOldStyleKeys not;
		keyText: 'r' if: Nautilus useOldStyleKeys;		
		action: [ target renameClassWithRefactoringEngine: target selectedClass theNonMetaClass ];
		parent: #'Trait Refactoring';
		order: 0.
	
	(aBuilder item: #'Remove...')
		action: [ target removeClassWithRefactoringEngine: target selectedClasses ];
		iconName: #removeIcon;
		parent: #'Trait Refactoring';
		order: 100;
		withSeparatorAfter.
		
	(aBuilder item: #'Realize')
		action: [ target refactor realizeClass ];
		parent: #'Trait Refactoring';
		order: 500.
]

{ #category : #'*Nautilus' }
TraitDescription >> renameMethodAllItem: aBuilder [

	^ (aBuilder item: #'Rename method (all)')
			keyText: 'r, m' if: Nautilus useOldStyleKeys not;
			keyText: 'r' if: Nautilus useOldStyleKeys;
			action: [ | target |
				target := aBuilder model.
				"scroll := target methodWidget vScrollValue."
				target refactor renameMethodFor: target selectedMethod originMethod.
				"target methodWidget vScrollValue: scroll"
				 ].
]

{ #category : #'*Nautilus' }
TraitDescription >> sourceCodeRefactoring: aBuilder [
	| target |
	target := aBuilder model.
	(aBuilder item: #'Create cascade')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor createCascadeBetween: target selectionInterval from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 0.
	(aBuilder item: #'Extract method')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor extractBetween: target selectionInterval from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 100.
	(aBuilder item: #'Extract method to component')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor extractToComponentBetween: target selectionInterval from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 200.
	(aBuilder item: #'Extract to temporary')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor extractToTemporaryBetween: target selectionInterval from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 300.
	(aBuilder item: #'Inline method')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor inlineMethodBetween: target selectionInterval from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 400.
	(aBuilder item: #'Inline method from component')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor
						inlineMethodFromComponentBetween: target selectionInterval
						from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 500.
	(aBuilder item: #'Inline temporary')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor inlineTemporaryBetween: target selectionInterval from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 600.
	(aBuilder item: #'Move variable definition')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor moveVariableDefinitionBetween: target selectionInterval from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 700.
	(aBuilder item: #'Rename temporary/parameter')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor
						renameTemporaryNamed: target contentSelection
						Between: target selectionInterval
						from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 800.
	(aBuilder item: #'Split cascade')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor splitCascadeBetween: target selectionInterval from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 900.
	(aBuilder item: #'Temporary to instvar')
		action: [ 
					target okToChange
						ifFalse: [ ^ target ].
					target refactor
						temporaryToInstanceVariableNamed: target contentSelection asString
						Between: target selectionInterval
						from: target selectedMethod ];
		parent: #'Source code refactoring';
		order: 1000;
		withSeparatorAfter.
	(aBuilder item: #Undo)
		order: 1100;
		label: target refactor undoLabel;
		action: [ target refactor undoOperation ];
		enabled: target refactor undoEnabled;
		parent: #'Source code refactoring'.
	(aBuilder item: #Redo)
		order: 1200;
		label: target refactor redoLabel;
		action: [ target refactor redoOperation ];
		enabled: target refactor redoEnabled;
		parent: #'Source code refactoring';
		withSeparatorAfter
]

{ #category : #'*Nautilus' }
TraitDescription >> sourceCodeRefactoringMenu: aBuilder [
	| target |
	target := aBuilder model.
	(aBuilder item: #Format)
		order: 1100;
		keyText: 'F';
		action: [ target refactor formatSourceCode ].
	(aBuilder item: #'Source code refactoring')
		order: 1101;
		arguments: {};
		withSeparatorAfter
]

{ #category : #'*Nautilus' }
TraitDescription >> varRefactoringSubMenu: aBuilder [
	
]
