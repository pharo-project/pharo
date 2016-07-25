actions
chooseAllUnchosenLocal
	"Notify the potential new state of canMerge."
	
	conflicts do: [ :ea | ea isResolved ifFalse: [ ea chooseLocal ] ].
	self changed: #text; changed: #list; changed: #canMerge