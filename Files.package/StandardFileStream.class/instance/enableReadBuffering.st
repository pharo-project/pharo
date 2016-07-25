private
enableReadBuffering

	collection ifNil: [
		buffer1 ifNotNil: [
			collection := self collectionSpecies new: 2048 ] ].
	readLimit := position := 0