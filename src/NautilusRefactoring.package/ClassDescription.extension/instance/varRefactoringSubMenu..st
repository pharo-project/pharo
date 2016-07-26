*NautilusRefactoring
varRefactoringSubMenu: aBuilder
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
		icon: (Smalltalk ui icons iconNamed: #removeIcon);
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