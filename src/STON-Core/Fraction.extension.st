Extension { #name : 'Fraction' }

{ #category : '*STON-Core' }
Fraction >> stonOn: stonWriter [
	stonWriter writeFraction: self
]
