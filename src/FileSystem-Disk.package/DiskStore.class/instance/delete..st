public
delete: path
	| pathString encodedPathString |
	
	(self exists: path)
		ifFalse: [ ^ FileDoesNotExist signalWith: path ].
		
	pathString := self stringFromPath: path.
	encodedPathString := Primitives encode: pathString.
	
	(self isDirectory: path)
		ifTrue: [ Primitives deleteDirectory: encodedPathString ]
		ifFalse: [ 
			File
				retryWithGC: [ Primitives deleteFile: encodedPathString ]
				until: [ :result | result notNil ]
				forFileNamed: pathString ]