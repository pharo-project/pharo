Extension { #name : #HelpTopic }

{ #category : #'*GT-Inspector' }
HelpTopic >> gtDisplayOn: stream [
	^ stream nextPutAll: self title
]
