refactory-changes
removeClassNamed: aSymbol 
	^ self addChange: (RBRemoveClassChange removeClassName: aSymbol)