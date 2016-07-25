menu
instVarRefactoringSubmenu: aBuilder
	<nautilusGlobalClassMenu>
	| target selectedClass |
	
	target := aBuilder model.
	selectedClass := target selectedClass.
	selectedClass ifNil: [^target].
	
	selectedClass instVarRefactoring: aBuilder.