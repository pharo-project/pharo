Extension { #name : 'CompiledCode' }

{ #category : '*Flashback-Decompiler' }
CompiledCode >> ir [
	"We as the AST for the IR... for decompiling ir from bytecode, look at IRBytecodeDecompiler"
	^ self ast ir
]

{ #category : '*Flashback-Decompiler' }
CompiledCode >> irPrimitive [

	| primNode n |
	primNode := OCIRPrimitive new num: (n := self primitive).
	(n = 117 or: [n = 120]) ifTrue: [
		primNode spec: (self literalAt: 1)].
	^ primNode
]
