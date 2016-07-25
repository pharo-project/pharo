updating
updateContentColor: paneColor
	"Change the content text color."
	
	self enabled
		ifTrue: [self contentMorph textColor: self theme textColor ]
		ifFalse: [self contentMorph textColor: self theme disabledColor ]