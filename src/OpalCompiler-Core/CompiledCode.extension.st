Extension { #name : #CompiledCode }

{ #category : #'*OpalCompiler-Core' }
CompiledCode >> ast [
	^ self subclassResponsibility
]

{ #category : #'*opalcompiler-core' }
CompiledCode >> compiler [
	^self methodClass compiler
]

{ #category : #'*opalcompiler-core' }
CompiledCode >> compilerClass [
	^self methodClass 
		ifNil: [Smalltalk compilerClass] 
		ifNotNil: [:class | class compilerClass].
]
