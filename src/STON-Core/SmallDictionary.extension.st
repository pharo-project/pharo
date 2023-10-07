Extension { #name : 'SmallDictionary' }

{ #category : '*STON-Core' }
SmallDictionary class >> fromSton: stonReader [
	"My instances are stored as maps."

	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary
]

{ #category : '*STON-Core' }
SmallDictionary >> stonOn: stonWriter [
	"I store my instances as maps"

	stonWriter
		writeObject: self
		do: [ stonWriter encodeMap: self ]
]
