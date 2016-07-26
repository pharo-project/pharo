comparing
endsWithAnyOf: aCollection
	aCollection do:[:suffix|
		(self endsWith: suffix) ifTrue:[^true].
	].
	^false