*opalcompiler-core
compiler
	^self methodClass 
		ifNil: [Smalltalk compiler] 
		ifNotNil: [:class | class compiler].