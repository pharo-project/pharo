Extension { #name : #Fraction }

{ #category : #'*ston-core' }
Fraction >> stonOn: stonWriter [
	stonWriter writeFraction: self
]
