accessing
keys

	^Array new: tally streamContents: [ :stream |
		self keysDo: [ :key | stream nextPut: key ] ]