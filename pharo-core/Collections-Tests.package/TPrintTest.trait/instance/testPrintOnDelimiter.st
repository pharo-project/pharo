tests - printing
testPrintOnDelimiter
	| aStream result allElementsAsString tmp |
	result:=''.
	aStream:= ReadWriteStream on: result.
	tmp:= OrderedCollection new.
	self nonEmpty do: [:each | tmp add: each asString].
	
	
	
	self nonEmpty printOn: aStream delimiter: ', ' .
	
	allElementsAsString:=(result findBetweenSubstrings: ', ' ).
	1 to: allElementsAsString size do:
		[:i | 
		self assert: (tmp occurrencesOf:(allElementsAsString at:i))=(allElementsAsString  occurrencesOf:(allElementsAsString at:i))
			].