Extension { #name : #Collection }

{ #category : #'*ston-core' }
Collection class >> fromSton: stonReader [
	"For collections we chose to instanciate based a list of elements using #add:
	This is not the best or most correct solution for all subclasses though,
	so some will revert to standard object behavior or chose another solution."
	
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection
]

{ #category : #'*ston-core' }
Collection >> stonOn: stonWriter [
	"For collections we chose to write a list of elements as delivered by #do:
	This is not the best or most correct solution for all subclasses though,
	so some will revert to standard object behavior or chose another solution"
	
	stonWriter writeObject: self do: [
		stonWriter encodeList: self ]

]
