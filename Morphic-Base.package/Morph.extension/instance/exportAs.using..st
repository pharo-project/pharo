*Morphic-Base
exportAs: anExtension using: aWriter
	
	| reference |
	
	reference := UIManager default 
		fileSave: 'Save Morph as ', anExtension asUppercase 
		initialAnswer: (self externalName, '.' , anExtension)
		extensions: { anExtension } 
		path: Smalltalk imageDirectory.
		
	reference ifNotNil: [ aWriter putForm: self imageForm onFileNamed: reference ]
	