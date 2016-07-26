initialization
initializeWidgets

	listModel := self newList.
	textModel := self newCode.
	toolbarModel := self instantiate: MethodToolbar.
	
	self focusOrder
		add: listModel;
		add: toolbarModel;
		add: textModel.
	
	textModel aboutToStyle: true.