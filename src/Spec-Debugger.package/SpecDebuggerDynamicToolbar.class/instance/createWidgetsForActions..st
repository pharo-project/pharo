building widgets
createWidgetsForActions: aCollection
	
	self instantiatePresenters: (self buildButtonWidgetsSpecForActions: aCollection).
	aCollection do: [ :action | 
		self initializeButtonWidget: (self widgets at: action specId) forAction: action ].
	self setFocusOrderForActions: aCollection