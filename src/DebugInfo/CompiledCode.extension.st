Extension { #name : 'CompiledCode' }

{ #category : '*DebugInfo' }
CompiledCode >> debugInfo [

	^ OnlineSourceDebugInfo for: self
]

{ #category : '*DebugInfo' }
CompiledCode >> symbolicBytecodes [
	"Answer Collection that contains of all the byte codes in a method as an instance of SymbolicInstruction"

	^SymbolicBytecodeBuilder decode: self
]
