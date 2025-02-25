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
