Extension { #name : 'Number' }

{ #category : '*STON-Core' }
Number >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Core' }
Number >> stonOn: stonWriter [
	stonWriter writeFloat: self asFloat
]
