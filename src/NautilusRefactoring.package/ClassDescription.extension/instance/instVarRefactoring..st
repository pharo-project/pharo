*NautilusRefactoring
instVarRefactoring: aBuilder
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