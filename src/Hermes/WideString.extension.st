Extension { #name : #WideString }

{ #category : #'*Hermes' }
WideString >> asExportedLiteral [
	^ HEWideString for:self.
]
