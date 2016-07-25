*NautilusRefactoring
methodRefactoring: aBuilder
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