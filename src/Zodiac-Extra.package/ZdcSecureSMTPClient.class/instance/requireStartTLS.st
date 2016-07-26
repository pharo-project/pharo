private protocol
requireStartTLS
	(self lastResponse includesSubstring: 'STARTTLS')
		ifFalse: [ ^ self error: 'Server does not seem to support STARTTLS' ].
