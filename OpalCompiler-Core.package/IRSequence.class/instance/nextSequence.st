successor sequences
nextSequence

	| sequences i |
	sequences := self withAllSuccessors.
	i := sequences findFirst: [:seq | seq orderNumber = self orderNumber].
	(i = 0 or: [i = sequences size]) ifTrue: [^ nil].
	^ sequences at: i + 1