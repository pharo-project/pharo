Extension { #name : #FileReference }

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> displayStringOn: stream [
	filesystem printPath: path on: stream
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtDisplayOn: stream [
	filesystem printPath: path on: stream
]
