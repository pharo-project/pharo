*NautilusRefactoring
refactoringMenu: aBuilder
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