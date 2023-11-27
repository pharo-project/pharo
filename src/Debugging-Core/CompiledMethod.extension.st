Extension { #name : 'CompiledMethod' }

{ #category : '*Debugging-Core' }
CompiledMethod >> decompileIR [

	^ IRBytecodeDecompiler new decompile: self
]
