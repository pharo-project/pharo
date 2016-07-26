actions
chooseAllNewerConflicts
	"Notify the potential new state of canMerge."
	
	conflicts do: [ :ea | ea chooseNewer ].
	self changed: #text; changed: #list; changed: #canMerge