private-history
addInHistory: aTab

	self removeFromHistory: aTab.
	selectionHistory addLast: aTab