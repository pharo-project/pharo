private-history
addFirstInHistory: aTab

	self removeFromHistory: aTab.
	selectionHistory addFirst: aTab