*MessageBrowserRefactoringAddition
methodRefactoringSubMenu: aBuilder
	<messageBrowserListMenu>
	| target |
	target := aBuilder model.
	target selectedMethod ifNil: [ ^ target ].
	aBuilder model selectedClass methodRefactoring: aBuilder