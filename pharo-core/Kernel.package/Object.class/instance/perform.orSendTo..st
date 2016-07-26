message performing
perform: selector orSendTo: otherTarget
	"If I wish to intercept and handle selector myself, do it; else send it to otherTarget"
	^ (self respondsTo: selector) ifTrue: [self perform: selector] ifFalse: [otherTarget perform: selector]