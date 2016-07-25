building widgets
setFocusOrderForActions: aCollection

	self focusOrder removeAll.
	aCollection do: [ :action | 
		self focusOrder add: (self perform: action specId) ].	
	self focusOrder add: (self perform: self notifierPaneWidgetId)