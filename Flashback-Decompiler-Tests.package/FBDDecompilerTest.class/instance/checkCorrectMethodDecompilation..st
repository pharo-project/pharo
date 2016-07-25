testing
checkCorrectMethodDecompilation: originalMethod
	| newMethod |
	newMethod := self decompileThenRecompile: originalMethod.
	self compare: originalMethod with: newMethod.