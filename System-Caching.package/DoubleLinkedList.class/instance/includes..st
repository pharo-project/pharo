testing
includes: object
	"Return true when I hold object as a value, false otherwise."
	
	self do: [ :each |
		each = object ifTrue: [ ^ true ] ].
	^ false