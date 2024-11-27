Extension { #name : 'Interval' }

{ #category : '*STON-Core' }
Interval class >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"

	^ self new
		fromSton: stonReader;
		yourself
]

{ #category : '*STON-Core' }
Interval >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"

	stonReader parseNamedInstVarsFor: self
]

{ #category : '*STON-Core' }
Interval >> stonOn: stonWriter [
	"Overwritten to get back the standard object behavior"

	stonWriter writeObject: self
]
