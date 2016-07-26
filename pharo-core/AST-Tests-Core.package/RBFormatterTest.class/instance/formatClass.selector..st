private
formatClass: aClass selector: aSymbol
	| source tree1 tree2 |
	source := aClass sourceCodeAt: aSymbol.
	tree1 := RBParser parseMethod: source.
	tree2 := RBParser
		parseMethod:
			(self formatterClass new format: tree1)
		onError: [ :err :pos | self assert: false ].
	self assert: tree1 = tree2