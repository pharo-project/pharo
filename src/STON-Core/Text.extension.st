Extension { #name : #Text }

{ #category : #'*ston-core' }
Text class >> fromSton: stonReader [
	^ self new
		fromSton: stonReader;
		yourself
]
