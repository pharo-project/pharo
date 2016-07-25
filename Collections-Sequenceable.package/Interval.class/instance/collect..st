enumerating
collect: aBlock
	| nextValue result |
	result := self species new: self size.
	nextValue := start.
	1 to: result size do:
		[:i |
		result at: i put: (aBlock value: nextValue).
		nextValue := nextValue + step].
	^ result