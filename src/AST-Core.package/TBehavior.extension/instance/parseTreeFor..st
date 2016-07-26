*ast-core
parseTreeFor: aSymbol	
	^ RBParser parseMethod: (self sourceCodeAt: aSymbol) onError: [ :msg :pos | ^ nil ]