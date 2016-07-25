protocol
doesNotUnderstand: aMessage

	^ (value respondsTo: aMessage selector)
		ifTrue: [ value perform: aMessage selector withEnoughArguments: aMessage arguments ]
		ifFalse: [ super doesNotUnderstand: aMessage ]