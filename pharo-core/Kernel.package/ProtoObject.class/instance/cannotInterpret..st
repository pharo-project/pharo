reflective operations
cannotInterpret: aMessage 
	 "Handle the fact that there was an attempt to send the given message to the receiver but a null methodDictionary was encountered while looking up the message selector.  Hopefully this is the result of encountering a stub for a swapped out class which induces this exception on purpose."

"If this is the result of encountering a swap-out stub, then simulating the lookup in Smalltalk should suffice to install the class properly, and the message may be resent."

	(self class lookupSelector: aMessage selector) ifNotNil:
		["Simulated lookup succeeded -- resend the message."
		^ aMessage sentTo: self].

	"Could not recover by simulated lookup -- it's an error"
	Error signal: 'MethodDictionary fault'.

	"Try again in case an error handler fixed things"
	^ aMessage sentTo: self