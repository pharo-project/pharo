private-actions
tabAddedToSelection: aTab

	aTab selected ifTrue: [ ^ self ].
	numberOfSelectedTabs := numberOfSelectedTabs + 1.
	self addInHistory: aTab.
	aTab silentlySelected: true.
	self adjustLayout.
	
	self triggerEvent: #tabAddedToSelection with: aTab