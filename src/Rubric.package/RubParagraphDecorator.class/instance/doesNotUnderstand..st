error handling
doesNotUnderstand: aMessage
	^ [ aMessage sendTo: next ]
		on: MessageNotUnderstood
		do: [ super doesNotUnderstand: aMessage ]