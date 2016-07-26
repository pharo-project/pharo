menu
refactoringClassMenu: aBuilder
	<nautilusGlobalClassMenu>
	| target |
	target := aBuilder model.
	target selectedClass ifNil: [ ^ target ].
	target selectedClass refactoringMenu: aBuilder