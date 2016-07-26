events-processing
handleListenEvent: anEvent
	"Handle the given event. This message is sent if the receiver is a registered listener for the given event."
	^anEvent sentTo: self.