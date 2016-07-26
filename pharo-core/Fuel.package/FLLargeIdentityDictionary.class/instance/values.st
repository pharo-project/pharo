accessing
values

	^Array new: tally streamContents: [ :stream |
		self valuesDo: [ :value | stream nextPut: value ] ]