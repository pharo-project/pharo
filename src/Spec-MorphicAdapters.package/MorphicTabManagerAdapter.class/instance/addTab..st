spec protocol
addTab: aTab
	self widgetDo: [ :w | w addTab: (self buildTab: aTab) ]