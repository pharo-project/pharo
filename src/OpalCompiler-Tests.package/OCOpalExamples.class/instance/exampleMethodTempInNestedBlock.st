examples-blocks
exampleMethodTempInNestedBlock
	| temp block |
	temp := 0.
	block := [ [ temp ] ].
	temp := 1.
	block := block value.
	temp := 2.
	"self assert: block value = 2."
	^block value.