actions
chooseLocal
	"Notify the potential new state of canMerge."
	
	self conflictSelectionDo:
		[selection chooseLocal.
		self changed: #text; changed: #list; changed: #canMerge]