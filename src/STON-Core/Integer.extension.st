Extension { #name : 'Integer' }

{ #category : '*STON-Core' }
Integer >> stonOn: stonWriter [
	stonWriter writeInteger: self
]
