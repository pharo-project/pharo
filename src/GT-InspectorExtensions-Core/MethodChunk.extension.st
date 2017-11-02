Extension { #name : #MethodChunk }

{ #category : #'*GT-InspectorExtensions-Core' }
MethodChunk >> gtDisplayOn: stream [
	stream nextPutAll: self description
]
