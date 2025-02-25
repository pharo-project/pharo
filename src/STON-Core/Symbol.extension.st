Extension { #name : 'Symbol' }

{ #category : '*STON-Core' }
Symbol >> stonOn: stonWriter [
	stonWriter writeSymbol: self
]
