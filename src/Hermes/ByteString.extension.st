Extension { #name : #ByteString }

{ #category : #'*Hermes' }
ByteString >> asExportedLiteral [
	^ HEByteString for:self.
]
