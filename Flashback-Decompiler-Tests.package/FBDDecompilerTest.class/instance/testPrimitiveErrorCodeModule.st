tests
testPrimitiveErrorCodeModule
	| newMethod |
	newMethod := self decompileThenRecompile: (self getMethod: #examplePrimitiveErrorCodeModule).