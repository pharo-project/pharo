private
unknownError: aDictionary
	^ String streamContents: [ :stream | 
		(aDictionary associations sorted: [ :a :b | a key < b key  ])
			do: [ :each | stream << each key << ': ' << each value ]
			separatedBy: [ stream << ', ' ] ]