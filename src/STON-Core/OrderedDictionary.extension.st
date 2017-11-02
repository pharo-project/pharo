Extension { #name : #OrderedDictionary }

{ #category : #'*ston-core' }
OrderedDictionary class >> fromSton: stonReader [
	"My instances are stored as maps."
	
	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary
]

{ #category : #'*ston-core' }
OrderedDictionary >> stonOn: stonWriter [
	"I store my instances as maps"
	
	stonWriter 
		writeObject: self 
		do: [ stonWriter encodeMap: self ]
]
