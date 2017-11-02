Extension { #name : #Number }

{ #category : #'*ston-core' }
Number >> stonContainSubObjects [ 
	^ false
]

{ #category : #'*ston-core' }
Number >> stonOn: stonWriter [
	stonWriter writeFloat: self asFloat
]
