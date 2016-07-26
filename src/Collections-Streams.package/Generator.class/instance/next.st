accessing
next
	"Generate and answer the next object in the receiver."

	^ self atEnd ifFalse: [
		home swapSender: thisContext sender.
		continue := thisContext swapSender: continue
	]