protocol
getList: aSymbolOrBlockWithOneArgument
	aSymbolOrBlockWithOneArgument isBlock
		ifTrue: [ listBlock := aSymbolOrBlockWithOneArgument. ^ self updateList ].
	aSymbolOrBlockWithOneArgument isSymbol 
		ifTrue: [ 
			listBlock := [ :regex| model perform: aSymbolOrBlockWithOneArgument with: regex].
			^ self updateList].
	Error signal: 'invalid argument'.