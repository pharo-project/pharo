accessing
upToEnd

	^self collectionSpecies
		new: self size - self position
		streamContents: [ :stream |
			| element |
			[ (element := self next) == nil ] whileFalse: [
				stream nextPut: element ] ]
