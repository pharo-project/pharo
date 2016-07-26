code generation
fieldOffset
	"Return temp or instVar offset for this variable"
	^index ifNil: [code < 256
					ifTrue: [code \\ 16]
					ifFalse: [code \\ 256]]