menu
refactoringMethodMenu: aBuilder
	<nautilusGlobalMethodMenu>
	| target |
	target := aBuilder model.
	target selectedMethod ifNil: [ ^ target ].
	target selectedMethod origin instanceSide refactoringMethod: aBuilder