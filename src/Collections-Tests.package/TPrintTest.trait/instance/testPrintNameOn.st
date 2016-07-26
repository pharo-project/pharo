tests - printing
testPrintNameOn

	| aStream result |
	result:=''.
	aStream:= ReadWriteStream on: result.
	self nonEmpty printNameOn: aStream.
	self nonEmpty class name first isVowel
		ifTrue:[ self assert: aStream contents =('an ',self nonEmpty class name ) ]
		ifFalse:[self assert: aStream contents =('a ',self nonEmpty class name)].