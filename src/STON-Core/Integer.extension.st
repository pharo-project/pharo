Extension { #name : #Integer }

{ #category : #'*ston-core' }
Integer >> stonOn: stonWriter [
	stonWriter writeInteger: self
]
