Extension { #name : #MethodChunk }

{ #category : #'*GT-InspectorExtensions-Core' }
MethodChunk >> displayStringOn: stream [
	stream nextPutAll: self description
]

{ #category : #'*GT-InspectorExtensions-Core' }
MethodChunk >> gtDisplayOn: stream [
	stream nextPutAll: self description
]
