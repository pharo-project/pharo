Extension { #name : #CompiledCode }

{ #category : #'*opalcompiler-core' }
CompiledCode >> compiler [
	^self methodClass 
		ifNil: [Smalltalk compiler] 
		ifNotNil: [:class | class compiler].
]

{ #category : #'*opalcompiler-core' }
CompiledCode >> compilerClass [
	^self methodClass 
		ifNil: [Smalltalk compilerClass] 
		ifNotNil: [:class | class compilerClass].
]
