Extension { #name : #CompiledMethod }

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledMethod >> gtDisplayOn: aStream [
	aStream print: self methodClass; nextPutAll: '>>'; store: self selector.
]
