private-actions
tabRemovedFromSelection: aTab

	numberOfSelectedTabs == 1 ifTrue: [ ^ self ].
	numberOfSelectedTabs := numberOfSelectedTabs - 1.

	"Insert the remove tab just before the selected ones to preserve order"
	selectionHistory remove: aTab.
	selectionHistory add: aTab afterIndex: (selectionHistory size - numberOfSelectedTabs).
	
	aTab silentlySelected: false.
	self adjustLayout.
	
	self triggerEvent: #tabRemovedFromSelection with: aTab