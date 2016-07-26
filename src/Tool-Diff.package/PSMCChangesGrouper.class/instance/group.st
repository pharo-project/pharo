grouping
group

	model do: [:operation | self visit: operation ].
	^ (nonClassChanges sorted: [:a :b | a item <= b item]),
		(classChanges sorted: [:a :b | a item <= b item])