Extension { #name : 'Text' }

{ #category : '*STON-Extensions' }
Text class >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"

	^ self new
		fromSton: stonReader;
		yourself
]

{ #category : '*STON-Extensions' }
Text >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"

	stonReader parseNamedInstVarsFor: self
]

{ #category : '*STON-Extensions' }
Text >> stonOn: stonWriter [
	"Overwritten to get back the standard object behavior"

	stonWriter writeObject: self
]
