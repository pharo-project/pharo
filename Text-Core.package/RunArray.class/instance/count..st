enumerating
count: aBlock
	"Beware, the block will be evaluated only once per group of values."
	| count |
	count := 0.
	self runsAndValuesDo: [:run :value |
		(aBlock value: value)
			ifTrue:
				[count := count + run]].
	^count