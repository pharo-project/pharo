menu
classVarRefactoringSubmenu: aBuilder
	<nautilusGlobalClassMenu>
	| target selectedClass |
	target := aBuilder model.
	selectedClass := target selectedClass.
	selectedClass ifNil: [ ^ target ].
	
	selectedClass varRefactoringSubMenu: aBuilder.

	