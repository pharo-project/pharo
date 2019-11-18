Extension { #name : #RBLiteralValueNode }

{ #category : #'*NECompletion' }
RBLiteralValueNode >> completionEntries [
	value isSymbol ifFalse: [ ^ #() ].
	"return all symbols that start with value"
	^Symbol allSymbols 
		select: [ :each | (each beginsWith: value) and: [each isLiteralSymbol]] 
		thenCollect: [ :each | NECEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
RBLiteralValueNode >> completionToken [
	^ self value asString
]
