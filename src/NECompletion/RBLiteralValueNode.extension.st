Extension { #name : #RBLiteralValueNode }

{ #category : #'*NECompletion' }
RBLiteralValueNode >> completionEntries: offset [
	value isSymbol ifFalse: [ ^ #() ].
	"return all symbols that start with value"
	^Symbol allSymbols 
		select: [ :each | (each beginsWith: value) and: [each isLiteralSymbol]] 
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
RBLiteralValueNode >> completionToken: offset [
	^ self value asString
]
