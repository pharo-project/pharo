*opalcompiler-core
methodNode
	"Return the parse tree that represents self"
	| aClass source methodNode |
	
	aClass := self methodClass.
	source := self sourceCode.
	methodNode := aClass compiler parse: source.
	methodNode source: source.
	^methodNode.
			