evaluating
ifError: errorHandlerBlock
	"Evaluate the block represented by the receiver, and normally return it's value.  If an error occurs, the errorHandlerBlock is evaluated, and it's value is instead returned.  The errorHandlerBlock must accept zero, one, or two parameters (the error message and the receiver)."
	"Examples:
		[1 whatsUpDoc] ifError: [:err :rcvr | 'huh?'].
		[1 / 0] ifError: [:err :rcvr |
			'ZeroDivide' = err
				ifTrue: [Float infinity]
				ifFalse: [self error: err]]
"

	^ self on: Error do: [:ex |
		errorHandlerBlock cull: ex description cull: ex receiver]