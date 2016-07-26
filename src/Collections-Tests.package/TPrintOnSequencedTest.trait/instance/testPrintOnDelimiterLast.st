tests - printing
testPrintOnDelimiterLast

	| aStream result allElementsAsString |
	result:=''.
	aStream:= ReadWriteStream on: result.
	
	self nonEmpty printOn: aStream delimiter: ', ' last: 'and'.
	
	allElementsAsString:=(result findBetweenSubstrings: ', ' ).
	1 to: allElementsAsString size do:
		[:i | 
		i<(allElementsAsString size-1 )
			ifTrue: [self assert: (allElementsAsString at:i)=((self nonEmpty at:i)asString)].
		i=(allElementsAsString size-1)
			ifTrue:[ self deny: (allElementsAsString at:i)=('and')asString].
		i=(allElementsAsString size)
			ifTrue: [self assert: (allElementsAsString at:i)=((self nonEmpty at:(i-1))asString)].
			].