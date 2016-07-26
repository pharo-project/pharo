compiling
compileTree: aBRMethodNode
	| method sourceCode change |
	sourceCode := aBRMethodNode newSource.
	change := self modelClass model
		compile: sourceCode
		in: self modelClass
		classified: self protocols.
	method := self class
		for: self modelClass
		source: sourceCode
		selector: aBRMethodNode selector.
	self modelClass addMethod: method.
	^ change