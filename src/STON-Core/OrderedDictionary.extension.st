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
	"I store my instances as maps. When in JSON mode, 
	encode me directly, without a class tag, keeping the order."
	
	stonWriter jsonMode 
		ifTrue: [ 
			stonWriter encodeMap: self ] 
		ifFalse: [ 
			stonWriter 
				writeObject: self 
				do: [ stonWriter encodeMap: self ] ]

]
