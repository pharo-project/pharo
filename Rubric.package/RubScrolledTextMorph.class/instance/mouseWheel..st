event handling
mouseWheel: event 
	"Handle a mouseWheel event."
	
	(self scrollPane scrollTarget handlesMouseWheel: event)
		ifTrue: [^self scrollTarget mouseWheel: event]. "pass on"