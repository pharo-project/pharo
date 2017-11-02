Extension { #name : #UndefinedObject }

{ #category : #'*ston-core' }
UndefinedObject >> stonContainSubObjects [ 
	^ false
]

{ #category : #'*ston-core' }
UndefinedObject >> stonOn: stonWriter [
	stonWriter writeNull
]
