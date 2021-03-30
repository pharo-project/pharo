Extension { #name : #GLMTransmission }

{ #category : #'*GT-InspectorExtensions-Core' }
GLMTransmission >> displayStringOn: stream [
	self origins asArray displayStringOn: stream.
	stream nextPutAll: '->'.
	self destination displayStringOn: stream
]

{ #category : #'*GT-InspectorExtensions-Core' }
GLMTransmission >> gtDisplayOn: stream [
	self origins asArray displayStringOn: stream.
	stream nextPutAll: '->'.
	self destination displayStringOn: stream
]
