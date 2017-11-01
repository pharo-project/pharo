Extension { #name : #RunArray }

{ #category : #'*ston-core' }
RunArray class >> fromSton: stonReader [
	^ self new
		fromSton: stonReader;
		yourself
]
