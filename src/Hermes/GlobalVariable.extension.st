Extension { #name : #GlobalVariable }

{ #category : #'*Hermes' }
GlobalVariable >> asExportedLiteral [
	^ HEGlobalVariable for:self.
]
