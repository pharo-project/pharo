Extension { #name : #ScaledDecimal }

{ #category : #'*Hermes' }
ScaledDecimal >> asExportedLiteral [
	^ HEScaledDecimal for: self.
]
