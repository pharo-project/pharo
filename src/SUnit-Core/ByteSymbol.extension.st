Extension { #name : #ByteSymbol }

{ #category : #'*SUnit-Core' }
ByteSymbol >> isTestSelector [
	self numArgs isZero
		ifFalse: [ ^ false ].
	"unary selectors starting with 'should' are supposed to be treated as test methods too"
	^(self beginsWith: 'test') or: [ self beginsWith: 'should' ]
]
