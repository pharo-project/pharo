Extension { #name : 'CompiledMethod' }

{ #category : '*Flashback-Decompiler' }
CompiledMethod >> decompileIR [

	^ FBIRBytecodeDecompiler new decompile: self
]
