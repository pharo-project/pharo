navigating
withExtension: extension 
	| basename name |
	basename := self basename.
	^ (basename endsWith: extension) 
		ifTrue: [ self ]
		ifFalse: 
			[name := basename copyUpToLast: self extensionDelimiter.
			self withName: name extension: extension]