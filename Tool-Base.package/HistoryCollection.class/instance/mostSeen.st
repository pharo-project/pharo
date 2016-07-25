protocol
mostSeen

	| max |
	max := storage first.
	2 to: storage size do: [:index || each |
		each := storage at: index.
		(each value > max value)
			ifTrue: [ max := each]].
	^ max key