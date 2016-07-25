protocol
lessSeen

	| min |
	min := storage first.
	2 to: storage size do: [:index || each |
		each := storage at: index.
		(each value < min value)
			ifTrue: [ min := each]].
	^ min key