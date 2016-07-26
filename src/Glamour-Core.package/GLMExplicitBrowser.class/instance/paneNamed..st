accessing
paneNamed: aSymbol 
	 
	^self panes detect: [:each | each name = aSymbol]