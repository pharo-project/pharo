menu
groupRefactoringMenu: aBuilder
	<nautilusGlobalGroupMenu>
	| target |
	target := aBuilder model.
	target selectedClass ifNotNil: [
		target selectedClass groupRefactoring: aBuilder ].