enumerating
findFirst: aBlock
	"Return the index of my first element for which aBlock evaluates as true."

	| index |
	index := 0.
	[(index := index + 1) <= self size] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0