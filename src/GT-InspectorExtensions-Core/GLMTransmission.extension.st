Extension { #name : #GLMTransmission }

{ #category : #'*GT-InspectorExtensions-Core' }
GLMTransmission >> gtDisplayOn: stream [
	self origins asArray gtDisplayOn: stream.
	stream nextPutAll: '->'.
	self destination gtDisplayOn: stream
]
