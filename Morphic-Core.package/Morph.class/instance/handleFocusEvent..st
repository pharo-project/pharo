events-processing
handleFocusEvent: anEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."
	^self handleEvent: anEvent