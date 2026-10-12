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
CompiledCode >> methodNode [
	^ self subclassResponsibility
]

{ #category : '*OpalCompiler-Core' }
CompiledCode >> sourceNode [
	^ self subclassResponsibility
]

{ #category : '*OpalCompiler-Core' }
CompiledCode >> sourceNodeForPC: aPC [

	| sourceNode |
	sourceNode := self sourceNode.
	^ sourceNode sourceNodeForPC: aPC
]
