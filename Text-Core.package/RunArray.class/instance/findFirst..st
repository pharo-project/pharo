enumerating
findFirst: aBlock
	| index |
	index := 1.
	self runsAndValuesDo: [ :run :value |
		(aBlock value: value) ifTrue: [^index].
		index := index + run].
	^0