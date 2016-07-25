tests - comma and delimiter
testAsCommaStringMore

	"self assert: self oneTwoThreeItemCol asCommaString = '1, 2, 3'.
	self assert: self oneTwoThreeItemCol asCommaStringAnd = '1, 2 and 3'
"

	| result resultAnd index allElementsAsString |
	result:= self nonEmpty asCommaString .
	resultAnd:= self nonEmpty asCommaStringAnd .
 
	index := 1.
	(result findBetweenSubstrings: ',' )do:
		[:each |
		index = 1
			ifTrue: [self assert: each= ((self nonEmpty at:index)asString)]
			ifFalse: [self assert: each= (' ',(self nonEmpty at:index) asString)].
		index:=index+1
		].
	
	"verifying esultAnd :"
	allElementsAsString:=(resultAnd findBetweenSubstrings: ',' ).
	1 to: allElementsAsString size do:
		[:i | 
		i<(allElementsAsString size )
			ifTrue: [
			i = 1
				ifTrue:[self assert: (allElementsAsString at:i)=((self nonEmpty at:i) asString)]
				ifFalse:[self assert: (allElementsAsString at:i)=(' ',(self nonEmpty at:i) asString)]
				].
		i=(allElementsAsString size)
			ifTrue:[ 
			i = 1
				ifTrue:[self assert: (allElementsAsString at:i)=( (self nonEmpty at:i ) asString ,' and ', (self nonEmpty at: ( i + 1) ) asString )]
				ifFalse:[self assert: (allElementsAsString at:i)=( ' ' , (self nonEmpty at:i ) asString ,' and ', (self nonEmpty at: ( i + 1) ) asString )]
				].
		
		
			].