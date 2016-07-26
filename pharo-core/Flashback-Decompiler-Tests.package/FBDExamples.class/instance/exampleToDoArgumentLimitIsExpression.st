examples-blocks-optimized
exampleToDoArgumentLimitIsExpression
	| count sum |
	count := 10.
	sum := 0.
	1 to: count - 1 do: [ :each | sum := sum + each].
	^sum