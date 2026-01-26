Extension { #name : 'RunArray' }

{ #category : '*STON-Extensions' }
RunArray class >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"

	^ self new
		fromSton: stonReader;
		yourself
]

{ #category : '*STON-Extensions' }
RunArray >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"

	stonReader parseNamedInstVarsFor: self
]

{ #category : '*STON-Extensions' }
RunArray >> stonOn: stonWriter [
	"Overwritten to get back the standard object behavior"

	stonWriter writeObject: self
]
