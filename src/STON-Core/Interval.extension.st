Extension { #name : #Interval }

{ #category : #'*ston-core' }
Interval class >> fromSton: stonReader [
	^ self new
		fromSton: stonReader;
		yourself
]

{ #category : #'*ston-core' }
Interval >> fromSton: stonReader [
	stonReader parseNamedInstVarsFor: self
]

{ #category : #'*STON-Core' }
Interval >> sort: aBlock [
	"Directly returns the Interval, since sorting an Interval does not make sense."
	^ self
]

{ #category : #'*ston-core' }
Interval >> stonOn: stonWriter [
	stonWriter writeObject: self
]
