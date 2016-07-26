private
decompileThenRecompile: originalMethod
	| methodNode |
	methodNode := FBDDecompiler new decompile: originalMethod.
	methodNode ifNil: [ ^ nil ].
	^ self exampleClass compiler compile: methodNode formattedCode.