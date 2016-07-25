*Glamour-Helpers
glamourValueWithArgs: anArray 
	 
	anArray size < 1 ifTrue: [^nil]. 
	^anArray first perform: self