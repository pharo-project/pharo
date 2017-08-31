tests parsing
testNumberRadixParsing
	2 to: 32 do: [ :radix | 
		| radixString |
		radixString := radix printString, 'r'.
		0 to: 72 do: [ :i | 
			self  assert: (self parserClass parseExpression: (radixString, (i radix: radix)))
				value = i ] ]