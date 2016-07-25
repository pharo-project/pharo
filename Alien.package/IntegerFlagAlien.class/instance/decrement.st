accessing
decrement
	| value |
	(value := self value) > 0 ifTrue:
		[self value: value - 1]