method accessing
bindingOf: aString 
	^self realClass classPool associationAt: aString asSymbol
		ifAbsent: [self realClass classPool associationAt: aString asString ifAbsent: [nil]]