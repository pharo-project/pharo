new selection
selectFrom: start to: stop
	"Select the specified characters inclusive."
	self selectInvisiblyFrom: start to: stop.
	self closeTypeIn.
	self storeSelectionInText.
	"Preserve current emphasis if selection is empty"
	stop > start 
		ifTrue: [self setEmphasisHereFromTextForward: true ]