building widgets
createButtonWidgetsForActions: aCollection

	self instantiateModels: (self buildButtonWidgetsSpecForActions: aCollection).
	aCollection do: [ :action | 
		self initializeButtonWidget: (self widgets at: action specId) forAction: action ]