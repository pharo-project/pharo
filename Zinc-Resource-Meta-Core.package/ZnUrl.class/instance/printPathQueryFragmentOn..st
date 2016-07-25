printing
printPathQueryFragmentOn: stream
	self isSchemeUsingPath 
		ifTrue: [ self printPathOn: stream ].
	self hasQuery
		ifTrue: [ self printQueryOn: stream ].
	self hasFragment
		ifFalse: [ ^ self ].
	stream nextPut: $#.
	self encodeQuery: self fragment on: stream