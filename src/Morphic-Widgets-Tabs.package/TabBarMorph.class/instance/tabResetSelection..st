private-actions
tabResetSelection: aTab
	"Clicked on an already clicked tab, since it can happened after a multi selection, I have to clear the selection except for this tab"
	
	selectionHistory ifNotEmpty: [
		(selectionHistory last: numberOfSelectedTabs) do: [ :each | each == aTab ifFalse: [ each silentlySelected: false ] ] ].
	
	numberOfSelectedTabs := 1.
	
	self addInHistory: aTab.
	self adjustLayout.
	
	self triggerEvent: #tabResetSelection with: aTab