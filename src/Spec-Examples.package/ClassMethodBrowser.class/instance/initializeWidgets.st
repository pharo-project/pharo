initialization
initializeWidgets

	listModel := self newList.
	methodModel := self instantiate: MethodBrowser.
	
	self focusOrder
		add: listModel;
		add: methodModel.
	
	methodModel displayBlock: [:method | method selector ].