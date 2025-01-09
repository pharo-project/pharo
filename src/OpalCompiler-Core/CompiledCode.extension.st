Extension { #name : 'CompiledCode' }

{ #category : '*OpalCompiler-Core' }
CompiledCode >> ast [
	^ self subclassResponsibility
]

{ #category : '*OpalCompiler-Core' }
CompiledCode >> compiler [
	^self methodClass compiler
]

{ #category : '*OpalCompiler-Core' }
CompiledCode >> compilerClass [
	^self methodClass
		ifNil: [Smalltalk compilerClass]
		ifNotNil: [:class | class compilerClass]
]

{ #category : '*OpalCompiler-Core' }
CompiledCode >> ir [
	"We as the AST for the IR... for decompiling ir from bytecode, look at IRBytecodeDecompiler"
	^ self ast ir
]

{ #category : '*OpalCompiler-Core' }
CompiledCode >> irPrimitive [

	| primNode n |
	primNode := OCIRPrimitive new num: (n := self primitive).
	(n = 117 or: [n = 120]) ifTrue: [
		primNode spec: (self literalAt: 1)].
	^ primNode
]
