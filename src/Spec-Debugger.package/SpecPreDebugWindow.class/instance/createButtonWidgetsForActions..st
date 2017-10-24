building widgets
createButtonWidgetsForActions: aCollection

	self instantiatePresenters: (self buildButtonWidgetsSpecForActions: aCollection).
	aCollection do: [ :action | 
		self initializeButtonWidget: (self widgets at: action specId) forAction: action ]