Extension { #name : #Integer }

{ #category : #'*Hermes' }
Integer >> asExportedLiteral [
	^ HEInteger for: self
]
