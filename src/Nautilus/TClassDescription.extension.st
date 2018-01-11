Extension { #name : #TClassDescription }

{ #category : #'*Nautilus' }
TClassDescription >> buildAnalyzeSubMenu: aBuilder [
	self explicitRequirement.
]

{ #category : #'*Nautilus' }
TClassDescription >> codeRewritingClass: aBuilder [
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
TClassDescription >> groupRefactoring: aBuilder [
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
TClassDescription >> instVarRefactoring: aBuilder [
	^self explicitRequirement.
]

{ #category : #'*Nautilus' }
TClassDescription >> methodRefactoring: aBuilder [
	^self explicitRequirement.
]

{ #category : #'*Nautilus' }
TClassDescription >> packageRefactoringMenu: aBuilder [
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
TClassDescription >> refactoringMenu: aBuilder [
	^self explicitRequirement.
]

{ #category : #'*Nautilus' }
TClassDescription >> refactoringMethod: aBuilder [

	(aBuilder item: #'Refactoring')
		order: -100.
			
	(self renameMethodAllItem: aBuilder)
		order: -90;
		withSeparatorAfter
]

{ #category : #'*Nautilus' }
TClassDescription >> refactoringSubmenu: aBuilder [
	^self explicitRequirement.
]

{ #category : #'*Nautilus' }
TClassDescription >> renameMethodAllItem: aBuilder [

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
TClassDescription >> sourceCodeRefactoring: aBuilder [
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
TClassDescription >> sourceCodeRefactoringMenu: aBuilder [
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
TClassDescription >> varRefactoringSubMenu: aBuilder [
	^self explicitRequirement.
]
