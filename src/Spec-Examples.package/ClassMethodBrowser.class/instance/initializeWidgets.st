initialization
initializeWidgets
	listModel := self instantiate:  ListPresenter .
	methodModel := self instantiate: MethodBrowser.
	self focusOrder
		add: listModel;
		add: methodModel.
	methodModel displayBlock: [ :method | method selector ]