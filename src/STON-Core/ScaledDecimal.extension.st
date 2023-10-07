Extension { #name : 'ScaledDecimal' }

{ #category : '*STON-Core' }
ScaledDecimal >> stonOn: stonWriter [
	stonWriter writeScaledDecimal: self
]
