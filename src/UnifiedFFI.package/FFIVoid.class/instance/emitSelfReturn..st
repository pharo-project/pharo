emitting code
emitSelfReturn: aBuilder
	^ aBuilder 
		popTop;
		pushReceiver;
		returnTop