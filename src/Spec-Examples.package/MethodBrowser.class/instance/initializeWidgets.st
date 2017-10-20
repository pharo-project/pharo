initialization
initializeWidgets
	listModel := self instantiate: ListPresenter .
	textModel := self newCode.
	toolbarModel := self instantiate: MethodToolbar.
	self focusOrder
		add: listModel;
		add: toolbarModel;
		add: textModel.
	textModel aboutToStyle: true