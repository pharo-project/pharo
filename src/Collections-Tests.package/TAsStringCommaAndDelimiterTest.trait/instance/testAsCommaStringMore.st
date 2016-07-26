tests - as string comma delimiter sequenceable
testAsCommaStringMore

	| result resultAnd allElementsAsString tmp |
	result:= self nonEmpty asCommaString .
	resultAnd:= self nonEmpty asCommaStringAnd .
	tmp :=OrderedCollection new. 
	self nonEmpty do: [ :each | tmp add: each asString].

	"verifying result  :"
	allElementsAsString := (result findBetweenSubstrings: ', ' ).
	allElementsAsString do:
		[:each |
		self assert: (tmp occurrencesOf: each)=(allElementsAsString occurrencesOf: each).
		].
	
	"verifying esultAnd :"
	allElementsAsString:=(resultAnd findBetweenSubstrings: ', ' ).
	1 to: allElementsAsString size do:
		[:i | 
		i<(allElementsAsString size-1 ) | (i= allElementsAsString size)
			ifTrue: [self assert: (tmp occurrencesOf:(allElementsAsString at:i))=(allElementsAsString  occurrencesOf:(allElementsAsString at:i))].
		i=(allElementsAsString size-1)
			ifTrue:[ self assert: (allElementsAsString at:i)=('and')].
			].