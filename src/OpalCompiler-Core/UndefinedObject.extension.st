Extension { #name : #UndefinedObject }

{ #category : #'*opalcompiler-core' }
UndefinedObject >> allTempNames [
	^#()
]

{ #category : #'*OpalCompiler-Core' }
UndefinedObject >> compiler [
	"Expressions (for example DoIts) are compiled with nil as the class. If we have this method, we can easily use the compiler that the class defines for syntax highlighting and code completion (without needing to add a check for nil)"
	^self class compilerClass new class: self
]
