Extension { #name : #RunArray }

{ #category : #'*STON-Text support' }
RunArray >> fromSton: stonReader [
	stonReader parseNamedInstVarsFor: self
]

{ #category : #'*STON-Text support' }
RunArray >> stonOn: stonWriter [
	stonWriter writeObject: self
]
