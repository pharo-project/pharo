menu
classRefactoringSubmenu: aBuilder
	<nautilusGlobalClassMenu>
	| target |
	target := aBuilder model.
	target selectedClass ifNil: [^target].
	
	target selectedClass refactoringSubmenu: aBuilder.