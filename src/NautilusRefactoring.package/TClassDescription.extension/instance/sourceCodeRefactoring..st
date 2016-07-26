*NautilusRefactoring
sourceCodeRefactoring: aBuilder
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