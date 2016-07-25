accessing
sharedVariablesFromString: aString 
	sharedVariables :=  (aString substrings: ' ') collect: [:x | x asSymbol => ClassVariable]. 