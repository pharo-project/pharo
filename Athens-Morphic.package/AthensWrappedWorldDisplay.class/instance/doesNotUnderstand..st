display protocol
doesNotUnderstand: aMessage
	"delegate all not implemented methods to the real display"

	^ (realDisplay respondsTo: aMessage selector)
		ifTrue: [ realDisplay perform: aMessage selector withEnoughArguments: aMessage arguments ]
		ifFalse: [ super doesNotUnderstand: aMessage ]