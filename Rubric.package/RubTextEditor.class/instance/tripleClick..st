events
tripleClick: evt
	self closeTypeIn.	"no matter what, if shift is pressed extend the selection"
	self selectLine.
	self setEmphasisHereFromText.
	self storeSelectionInText.