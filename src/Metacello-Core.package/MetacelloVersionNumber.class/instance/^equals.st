comparing
= aMetacelloVersionNumber

	| condensed aCondensed |
	aMetacelloVersionNumber species = self species
		ifFalse: [ ^ false ].
	condensed := self collapseZeros.
	aCondensed := aMetacelloVersionNumber collapseZeros.
	(condensed ~~ self or: [ aCondensed ~~ aMetacelloVersionNumber ])
		ifTrue: [ ^ condensed compareEqualTo: aCondensed ].
	^ self compareEqualTo: aMetacelloVersionNumber