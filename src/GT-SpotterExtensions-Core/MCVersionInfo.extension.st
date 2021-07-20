Extension { #name : #MCVersionInfo }

{ #category : #'*GT-SpotterExtensions-Core' }
MCVersionInfo >> gtDisplayOn: stream [
	stream nextPutAll: self name
]
