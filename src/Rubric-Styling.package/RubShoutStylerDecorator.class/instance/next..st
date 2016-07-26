accessing
next: aDecorator
	super next: aDecorator.
	aDecorator ifNotNil: [ 
		self paragraph textArea announcer
			when: RubTextAcceptRequest
			send: #whenTextAcceptRequest:
			to: self 
	]