cascading
cascadeReceiver
	"Nil out rcvr (to indicate cascade) and return what it had been."

	| rcvr |
	rcvr := receiver.
	receiver := nil.
	^rcvr