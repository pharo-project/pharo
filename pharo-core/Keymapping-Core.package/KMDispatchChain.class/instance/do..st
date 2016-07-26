iterating
do: aBlock
	| currentTarget |
	currentTarget := initialTarget.
	[ currentTarget isNil ]
		whileFalse: [
			aBlock value: currentTarget.
			currentTarget := currentTarget nextForKmChain: self.
		]