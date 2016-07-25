private
linkAt: index ifAbsent: errorBlock

	| i |
	i := 0.
	self linksDo: [:link |
		(i := i + 1) = index ifTrue: [^ link]].
	^ errorBlock value