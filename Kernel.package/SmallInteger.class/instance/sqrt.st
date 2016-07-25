mathematical functions
sqrt
	self negative ifTrue: [
		^ DomainError signal: 'sqrt undefined for number less than zero.' ].
	^super sqrt