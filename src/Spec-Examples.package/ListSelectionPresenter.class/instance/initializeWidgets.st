initialization
initializeWidgets
	listModel := self newList.
	textModel1 := self newText.
	textModel2 := self newCode.
	listModel beMultipleSelection.
	listModel items: Smalltalk globals allClasses