initialization
initializeWidgets
	listModel := self instantiate:  ListModel .
	methodModel := self instantiate: MethodBrowser.
	self focusOrder
		add: listModel;
		add: methodModel.
	methodModel displayBlock: [ :method | method selector ]