tests - printing
testPrintOn
	| aStream result allElementsAsString |
	result:=''.
	aStream:= ReadWriteStream on: result.
	
	self nonEmpty printOn: aStream .
	allElementsAsString:=(result findBetweenSubstrings: ' ' ).
	1 to: allElementsAsString size do:
		[:i | 
		i=1
			ifTrue:[
			self accessCollection class name first isVowel 
				ifTrue:[self assert: (allElementsAsString at:i)='an' ]
				ifFalse:[self assert: (allElementsAsString at:i)='a'].].
		i=2
			ifTrue:[self assert: (allElementsAsString at:i)=self accessCollection class name].
		i>2
			ifTrue:[self assert: (allElementsAsString at:i)=((self nonEmpty at:i)asString).].	
			].