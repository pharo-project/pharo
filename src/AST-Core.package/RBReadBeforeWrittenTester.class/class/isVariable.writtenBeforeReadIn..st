accessing
isVariable: aString writtenBeforeReadIn: aBRProgramNode 
	^(self readBeforeWritten: (Array with: aString) in: aBRProgramNode) 
		isEmpty