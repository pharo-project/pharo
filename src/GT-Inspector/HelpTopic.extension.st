Extension { #name : #HelpTopic }

{ #category : #'*GT-Inspector' }
HelpTopic >> displayStringOn: stream [
	^ stream nextPutAll: self title
]

{ #category : #'*GT-Inspector' }
HelpTopic >> gtDisplayOn: stream [
	^ stream nextPutAll: self title
]
