private-changes
compile: aString in: aRBClass classified: aSymbol 
	| change |
	change := changes 
				compile: aString
				in: aRBClass
				classified: aSymbol.
	self flushCaches.
	^change