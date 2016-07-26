helpers
menuOpenedDuring: aBlock

	| jailor |
	jailor := MenuCapturingMorph new openInWorld.
	aBlock value.
	^ jailor menu.