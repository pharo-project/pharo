enumerating
do: aBlock
	"Evaluate aBlock for each value of the interval.
	Implementation note: instead of repeatedly incrementing the value
		aValue := aValue + step.
	until stop is reached,
	We prefer to recompute value from start
		aValue := start + (index * step).
	This is better for floating points accuracy, while not degrading Integer and Fraction speed too much.
	Moreover, this is consistent with methods #at: and #size"

	| aValue index size |
	index := 0.
	size := self size.
	[index < size]
		whileTrue: [aValue := start + (index * step).
			index := index + 1.
			aBlock value: aValue]