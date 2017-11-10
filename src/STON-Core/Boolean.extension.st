Extension { #name : #Boolean }

{ #category : #'*ston-core' }
Boolean >> stonContainSubObjects [ 
	^ false
]

{ #category : #'*ston-core' }
Boolean >> stonOn: stonWriter [
	stonWriter writeBoolean: self
]
