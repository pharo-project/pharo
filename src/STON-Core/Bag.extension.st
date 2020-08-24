Extension { #name : #Bag }

{ #category : #'*STON-Core' }
Bag class >> fromSton: stonReader [
	"Read a map representation containing element/occurrences pairs"
	
	| bag |
	bag := self new.
	stonReader parseMapDo: [ :key :value |
		bag add: key withOccurrences: value ].
	^ bag
]

{ #category : #'*STON-Core' }
Bag >> stonOn: stonWriter [
	"Use a map with element-occurrences pairs as representation"

	stonWriter 
		writeObject: self 
		do: [ stonWriter encodeMap: contents ]
]
