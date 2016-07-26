valid-context
testTetModelIsAValidContext
	| textModel |
	
	(textModel := TextModel new)
		beForCode.
	self assert: (SugsAbstractContext contextFor: textModel) class equals: SugsMessageBrowserContext.