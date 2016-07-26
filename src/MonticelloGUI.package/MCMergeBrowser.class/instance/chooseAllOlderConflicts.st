actions
chooseAllOlderConflicts
	"Notify the potential new state of canMerge."
	
	conflicts do: [ :ea | ea chooseOlder ].
	self changed: #text; changed: #list; changed: #canMerge