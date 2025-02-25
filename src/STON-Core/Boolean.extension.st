Extension { #name : 'Boolean' }

{ #category : '*STON-Core' }
Boolean >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Core' }
Boolean >> stonOn: stonWriter [
	stonWriter writeBoolean: self
]
