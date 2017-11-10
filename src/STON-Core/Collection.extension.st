Extension { #name : #Collection }

{ #category : #'*ston-core' }
Collection class >> fromSton: stonReader [
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection
]

{ #category : #'*ston-core' }
Collection >> stonOn: stonWriter [
	stonWriter writeObject: self do: [
		stonWriter encodeList: self ]

]
