*opalcompiler-core
compilerClass
	^self methodClass 
		ifNil: [Smalltalk compilerClass] 
		ifNotNil: [:class | class compilerClass].