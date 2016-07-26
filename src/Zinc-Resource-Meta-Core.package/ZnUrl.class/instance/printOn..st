printing
printOn: stream
	self hasScheme ifTrue: [ 
		stream nextPutAll: self scheme; nextPut: $:.
		self isSchemeUsingDoubleSlash ifTrue: [ stream nextPut: $/; nextPut: $/ ] ].
	self printAuthorityOn: stream.
	self printPathQueryFragmentOn: stream