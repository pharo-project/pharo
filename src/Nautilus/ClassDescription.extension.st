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

{ #category : #'*Nautilus' }
ClassDescription >> codeRewritingClass: aBuilder [
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
ClassDescription >> groupRefactoring: aBuilder [
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
ClassDescription >> instVarRefactoring: aBuilder [
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Add')
		action: [ target refactor addInstVarFrom: self ];
		parent: #'Inst Var Refactoring';
		order: 0.
		
	(aBuilder item: #'Rename')
		action: [ target refactor renameInstVarFrom: self ];
		parent: #'Inst Var Refactoring';
		order: 100.
	
	(aBuilder item: #'Remove')
		action: [ target refactor removeInstVarFrom: self ];
		parent: #'Inst Var Refactoring';
		iconName: #removeIcon;
		order: 200;
		withSeparatorAfter.
		
	(aBuilder item: #'Abstract')
		action: [ target refactor abstractInstVarFrom: self ];
		parent: #'Inst Var Refactoring';
		order: 300.
		
	(aBuilder item: #'Accessors')
		action: [ target refactor accessorsInstVarFrom: self ];
		parent: #'Inst Var Refactoring';
		order: 400.
		
	(aBuilder item: #'Move to class')
		action: [ target refactor moveInstVarToClass: self ];
		parent: #'Inst Var Refactoring';
		order: 450.
	
	(aBuilder item: #'Pull up')
		action: [ target refactor pullUpInstVarFrom: self ];
		parent: #'Inst Var Refactoring';
		order: 500.
	
	(aBuilder item: #'Push down')
		action: [ target refactor pushDownInstVarFrom: self ];
		parent: #'Inst Var Refactoring';
		order: 600.
]

{ #category : #'*Nautilus' }
ClassDescription >> methodRefactoring: aBuilder [
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
			
	(aBuilder item: #'Swap')
		action: [ target refactor swapMethodFor: selectedMethods ];
		order: 400;
		label: (target selectedClass isMeta
					ifTrue: [ 'Move to instance side' ]
					ifFalse:[ 'Move to class side']);
		parent: #'Refactoring'.
		
	(aBuilder item: #'Push up')
		action: [ target refactor pullUpMethodsFor: selectedMethods ];
		order: 500;
		parent: #'Refactoring'.
			
	(aBuilder item: #'Push down')
		action: [ target refactor pushDownMethodsFor: selectedMethods ];
		order: 600;
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
ClassDescription >> packageRefactoringMenu: aBuilder [
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
ClassDescription >> refactoringMenu: aBuilder [
	| target |
	
	target := aBuilder model.
	
	(aBuilder item: #'Refactoring')
			order: -100.
	
	(aBuilder item: #'Rename inst var')
			action: [ target refactor renameInstVarFrom: self ];
			order: -98.
				
	(aBuilder item: #'Rename class var')
			action: [ target refactor renameClassVarFrom: self ];
			order: -97.
		
	(aBuilder item: #'Code Rewriting')
			order: 0;
			parent: #'Refactoring'.
			
	(aBuilder item: #'Class Refactoring')
			order: 100;
			parent: #'Refactoring';
			withSeparatorAfter.
			
	(aBuilder item: #'Class Var Refactoring')
			order: 200;
			parent: #'Refactoring'.
			
	(aBuilder item: #'Inst Var Refactoring')
			order: 300;
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
ClassDescription >> refactoringMethod: aBuilder [

	(aBuilder item: #'Refactoring')
		order: -100.
			
	(self renameMethodAllItem: aBuilder)
		order: -90;
		withSeparatorAfter
]

{ #category : #'*Nautilus' }
ClassDescription >> refactoringSubmenu: aBuilder [
	| target |
	target := aBuilder model.

	(aBuilder item: #'Rename...')
		keyText: 'r, c' if: Nautilus useOldStyleKeys not;
		keyText: 'r' if: Nautilus useOldStyleKeys;		
		action: [ target renameClassWithRefactoringEngine: target selectedClass theNonMetaClass ];
		parent: #'Class Refactoring';
		order: 0.
	
	(aBuilder item: #'Remove...')
		action: [ target removeClassWithRefactoringEngine: target selectedClasses ];
		iconName: #removeIcon;
		parent: #'Class Refactoring';
		order: 100;
		withSeparatorAfter.
		
	(aBuilder item: #'Generate Accessors')
		keyText: 'h, a' if: Nautilus useOldStyleKeys not;
		action: [ target refactor generateAccessors ];
		parent: #'Class Refactoring';
		order: 200.
		
	(aBuilder item: #'Generate Superclass...')
		action: [ target refactor generateSuperClass ];
		parent: #'Class Refactoring';
		order: 300.
	
	(aBuilder item: #'Insert superclass')
		action: [ target refactor insertSuperclass ];
		parent: #'Class Refactoring';
		order: 325.
		
	(aBuilder item: #'Insert subclass')
		action: [ target refactor insertSubclass ];
		parent: #'Class Refactoring';
		order: 350.
		
	(aBuilder item: #'Generate Subclass...')
		action: [ target refactor generateSubclass ];
		parent: #'Class Refactoring';
		order: 400.
		
	(aBuilder item: #'Realize')
		action: [ target refactor realizeClass ];
		parent: #'Class Refactoring';
		order: 500.
		
	(aBuilder item: #'Split')
		action: [ target refactor splitClass: target selectedClass ];
		parent: #'Class Refactoring';
		order: 600.
]

{ #category : #'*Nautilus' }
ClassDescription >> renameMethodAllItem: aBuilder [

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
ClassDescription >> sourceCodeRefactoring: aBuilder [
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
ClassDescription >> sourceCodeRefactoringMenu: aBuilder [
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
ClassDescription >> varRefactoringSubMenu: aBuilder [
	| target | 
	target := aBuilder model.
	
	(aBuilder item: #'Add')
		action: [ target refactor addClassVarFrom: self  ];
		parent: #'Class Var Refactoring';
		order: 0.
		
	(aBuilder item: #'Rename')
		action: [ target refactor renameClassVarFrom: self ];
		parent: #'Class Var Refactoring';
		order: 100.
	
	(aBuilder item: #'Remove')
		action: [ target refactor removeClassVarFrom: self ];
		parent: #'Class Var Refactoring';
		iconName: #removeIcon;
		order: 200;
		withSeparatorAfter.
		
	(aBuilder item: #'Abstract')
		action: [ target refactor abstractClassVarFrom: self ];
		parent: #'Class Var Refactoring';
		order: 300.
	
	(aBuilder item: #'Accessors')
		action: [ target refactor accessorsClassVarFrom: self ];
		parent: #'Class Var Refactoring';
		order: 400.
		
	(aBuilder item: #'Pull up')
		action: [ target refactor pullUpClassVarFrom: self ];
		parent: #'Class Var Refactoring';
		order: 500.
	
	(aBuilder item: #'Push down')
		action: [ target refactor pushDownClassVarFrom: self ];
		parent: #'Class Var Refactoring';
		order: 600.
]
