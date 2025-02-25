Extension { #name : 'String' }

{ #category : '*STON-Core' }
String >> stonContainSubObjects [
	^ false
]

{ #category : '*STON-Core' }
String >> stonOn: stonWriter [
	stonWriter writeString: self
]
