accessing
sharedVariablesFromString: aString 
	layoutDefinition sharedVariables:((aString substrings: ' ') collect: [:x | x asSymbol => ClassVariable]). 