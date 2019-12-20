Extension { #name : #Interval }

{ #category : #'*ston-core' }
Interval >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"
	
	stonReader parseNamedInstVarsFor: self
]

{ #category : #'*ston-core' }
Interval class >> fromSton: stonReader [
	"Overwritten to get back the standard object behavior"
	
	^ self new
		fromSton: stonReader;
		yourself
]

{ #category : #'*ston-core' }
Interval >> stonOn: stonWriter [
	"Overwritten to get back the standard object behavior"

	stonWriter writeObject: self
]
