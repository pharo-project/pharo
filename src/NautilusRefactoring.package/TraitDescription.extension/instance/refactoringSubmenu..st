*NautilusRefactoring
refactoringSubmenu: aBuilder
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