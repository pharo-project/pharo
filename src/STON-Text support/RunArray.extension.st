Extension { #name : #RunArray }

{ #category : #'*STON-Text support' }
RunArray class >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"

	^ self new
		fromSton: stonReader;
		yourself
]

{ #category : #'*STON-Text support' }
RunArray >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"
	
	stonReader parseNamedInstVarsFor: self
]

{ #category : #'*STON-Text support' }
RunArray >> stonOn: stonWriter [
	"Overwritten to get back the standard object behavior"
	
	stonWriter writeObject: self
]
