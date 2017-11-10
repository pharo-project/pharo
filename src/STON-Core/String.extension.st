Extension { #name : #String }

{ #category : #'*ston-core' }
String >> stonContainSubObjects [ 
	^ false
]

{ #category : #'*ston-core' }
String >> stonOn: stonWriter [
	stonWriter writeString: self
]
