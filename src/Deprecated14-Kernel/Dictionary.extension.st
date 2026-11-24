Extension { #name : 'Dictionary' }

{ #category : '*Deprecated14-Kernel' }
Dictionary >> removeUnreferencedKeys [
	"This is private code for the Undeclared handling, not a general API."

	self
		deprecated: 'Use "Smalltalk image cleanOutUndeclared" instead'
		in: #Pharo14
		transformWith: '`@rcv removeUnreferencedKeys' -> 'Smalltalk image cleanOutUndeclared'.
	Smalltalk image cleanOutUndeclared
]
