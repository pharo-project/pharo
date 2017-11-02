Extension { #name : #ByteArray }

{ #category : #'*Hermes' }
ByteArray >> asExportedLiteral [
	^ HEByteArray for:self.
]
