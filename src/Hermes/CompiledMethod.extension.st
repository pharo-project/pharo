Extension { #name : #CompiledMethod }

{ #category : #'*Hermes' }
CompiledMethod >> asExportedLiteral [
	^ HEMethodLiteral for:self.
]
