error handling
doesNotUnderstand: aMessage
	"{aMessage . thisContext sender} inspectOnce."
	^ aMessage sendTo: self editingMode