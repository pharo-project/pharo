Extension { #name : #Text }

{ #category : #'*STON-Text support' }
Text >> fromSton: stonReader [
	stonReader parseNamedInstVarsFor: self
]

{ #category : #'*STON-Text support' }
Text >> stonOn: stonWriter [
	stonWriter writeObject: self
]
