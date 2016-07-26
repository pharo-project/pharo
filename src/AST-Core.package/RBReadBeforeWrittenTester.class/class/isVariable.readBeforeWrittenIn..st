accessing
isVariable: aString readBeforeWrittenIn: aBRProgramNode 
	^(self isVariable: aString writtenBeforeReadIn: aBRProgramNode) not