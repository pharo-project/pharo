tests
testPrimitiveErrorCodeModule2
	| newMethod |
	newMethod := self decompileThenRecompile: (self getMethod: #examplePrimitiveErrorCodeModule2).