tests - valid context
testTetModelIsAValidContext
	| textModel |
	
	(textModel := TextPresenter new)
		beForCode.
	self assert: (SugsAbstractContext contextFor: textModel) class equals: SugsMessageBrowserContext.