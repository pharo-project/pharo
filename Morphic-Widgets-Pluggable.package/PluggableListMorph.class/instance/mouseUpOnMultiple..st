multi-selection
mouseUpOnMultiple: event
	"Reset the mouseDownRow."
	
	dragOnOrOff := nil.  "So improperly started drags will have not effect".
	event hand hasSubmorphs ifFalse: [
		self mouseDownRow: nil]