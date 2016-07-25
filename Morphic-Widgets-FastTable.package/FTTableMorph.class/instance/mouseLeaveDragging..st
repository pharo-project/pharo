event handling
mouseLeaveDragging: event
	"The mouse has left with a button down."
	
	(self dropEnabled and: [event hand hasSubmorphs]) 
		ifFalse: [ "no d&d" ^ super mouseLeaveDragging: event].
		
	self basicHighlightRowIndexes: #().
	self refresh
				