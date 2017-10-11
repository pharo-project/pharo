api
removeTab: aTab
	self basicRemoveTab: aTab.
	self changed: #removeTab: with: {aTab}