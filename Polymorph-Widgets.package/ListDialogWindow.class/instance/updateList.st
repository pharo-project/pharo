morphic protocol
updateList
	"update the displayed list in a separate thread to avoid UI blocking"
	
	"if there is already a background thread running for the new list discard it"
	listCreationProcess ifNotNil: [
		listCreationProcess terminate].
	
	"no pattern given => empty list"
	pattern ifNil: [ ^ list :=#() ].
	
	"fork off a possibly costly list calculation"
	listCreationProcess := [
		list := listBlock value: pattern.
		"make sure the ui is updated in a synchronized manner"
		self defer: [self listChanged]] fork.