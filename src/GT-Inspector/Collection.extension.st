Extension { #name : #Collection }

{ #category : #'*GT-Inspector' }
Collection >> gtInspectorHash [

	^ super gtInspectorHash bitXor: self size hash
]

{ #category : #'*GT-Inspector' }
Collection >> gtInspectorInterestingObject [
	^ self size = 1
		ifTrue: [ self anyOne ]
		ifFalse: [ self ]
]
