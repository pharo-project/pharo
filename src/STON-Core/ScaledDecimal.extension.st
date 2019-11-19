Extension { #name : #ScaledDecimal }

{ #category : #'*ston-core' }
ScaledDecimal >> stonOn: stonWriter [
	stonWriter writeScaledDecimal: self
]
