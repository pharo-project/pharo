private-actions
tabRefreshed: aTab

	self selectedTab = aTab ifFalse: [ ^ self ].

	self triggerEvent: #tabRefreshed with: aTab