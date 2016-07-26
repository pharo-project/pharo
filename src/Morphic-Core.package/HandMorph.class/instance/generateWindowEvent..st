private events
generateWindowEvent: evtBuf 
	"Generate the appropriate window event for the given raw event buffer"

	| evt |
	evt := WindowEvent new.
	evt setTimeStamp: evtBuf second.
	evt timeStamp = 0 ifTrue: [evt setTimeStamp: Time millisecondClockValue].
	evt action: evtBuf third.
	evt rectangle: (Rectangle origin: evtBuf fourth @ evtBuf fifth corner: evtBuf sixth @ evtBuf seventh ).
	
	^evt
