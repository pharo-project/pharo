private
convertToString: array

	^String streamContents: [ :stream |
		array do: [ :each |
			stream nextPutAll: each asString; cr ] ]