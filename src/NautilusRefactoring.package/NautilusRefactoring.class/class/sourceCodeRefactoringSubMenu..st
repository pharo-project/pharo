menu
sourceCodeRefactoringSubMenu: aBuilder
	<nautilusGlobalSourceCodeMenu>
	| target |
	target := aBuilder model.
	target ifNil: [ ^ self ].
	target selectedMethod ifNil: [ ^ self ].
	target selectedClass
		ifNotNil: [ :selectedClass | selectedClass sourceCodeRefactoring: aBuilder ]