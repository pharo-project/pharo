private
actionsChanged: aTab

	self selectedTab = aTab ifFalse: [ ^ self ].
	self adjustLayout.
	