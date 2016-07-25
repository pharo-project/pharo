private-actions
tabEmptyContents: aTab

	self selectedTab = aTab ifFalse: [ ^ self ].

	(selectionHistory last: numberOfSelectedTabs) do: [ :each | each == aTab ifFalse: [ each silentlySelected: false ] ].	
	numberOfSelectedTabs := 1.
	self adjustLayout.
	
	self triggerEvent: #tabEmptyContents with: aTab