Extension { #name : #Text }

{ #category : #'*STON-Text support' }
Text >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"
	
	stonReader parseNamedInstVarsFor: self
]

{ #category : #'*STON-Text support' }
Text class >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"
	
	^ self new
		fromSton: stonReader;
		yourself
]

{ #category : #'*STON-Text support' }
Text >> stonOn: stonWriter [
	"Overwritten to get back the standard object behavior"
	
	stonWriter writeObject: self
]
