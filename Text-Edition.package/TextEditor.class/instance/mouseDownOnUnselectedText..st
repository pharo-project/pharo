events
mouseDownOnUnselectedText: event
	self editingState mouseDown: event from: morph.
	self closeTypeIn.	"no matter what, if shift is pressed extend the selection"
	"make sure our view knows about selection change"	
	self morph handleInteraction: [ self handleDownOnUnselectedText: event ]