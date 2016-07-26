actions
chooseRemote
	"Notify the potential new state of canMerge."
	
	self conflictSelectionDo:
		[selection chooseRemote.
		self changed: #text; changed: #list; changed: #canMerge]