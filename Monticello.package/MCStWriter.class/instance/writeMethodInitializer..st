writing
writeMethodInitializer: aMethodDefinition
	
	aMethodDefinition isInitializer ifTrue: [ 
		self chunkContents: [ :str |
			str nextPutAll: aMethodDefinition className, ' initialize'.
			initializers add: (aMethodDefinition className -> str) ]
		].