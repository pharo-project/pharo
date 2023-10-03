Extension { #name : 'UndefinedObject' }

{ #category : '*STON-Core' }
UndefinedObject >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Core' }
UndefinedObject >> stonOn: stonWriter [
	stonWriter writeNull
]
