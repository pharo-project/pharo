refactory-changes
removeClassNamed: aSymbol 
	^ self addChange: (RBRefactoryChangeManager changeFactory removeClassNamed: aSymbol)