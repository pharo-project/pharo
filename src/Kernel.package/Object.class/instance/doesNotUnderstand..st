reflective operations
doesNotUnderstand: aMessage 
	 "Handle the fact that there was an attempt to send the given message to the receiver but the receiver does not understand this message (typically sent from the machine when a message is sent to the receiver and no method is defined for that selector)."
	"Testing: (3 activeProcess)"
		
	| exception resumeValue |
	(exception := MessageNotUnderstood new)
		message: aMessage;
		receiver: self.
	resumeValue := exception signal.
	^exception reachedDefaultHandler
		ifTrue: [aMessage sentTo: self]
		ifFalse: [resumeValue]