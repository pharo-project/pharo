Extension { #name : #ByteSymbol }

{ #category : #'*Hermes' }
ByteSymbol >> asExportedLiteral [
	^ HESymbol for: self.
]
