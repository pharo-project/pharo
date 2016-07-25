commands
list
	| handlers shortNames maxShortNameSize |
	self stdout 
		nextPutAll: 'Currently installed Command Line Handlers:'; lf.

	handlers := self allHandlers reject: [ :cls | 
			cls includesBehavior: BasicCommandLineHandler ].
	shortNames := handlers collect: #commandName.
	maxShortNameSize := (shortNames detectMax: [ :name|  name size ]) size.
	
	handlers do: [ :handler|
		self stdout 
			nextPutAll: '    ';
			nextPutAll: (handler commandName padRightTo: maxShortNameSize); 
			nextPutAll: '    ';
			nextPutAll: (handler description ifNil: [ '--']); lf].
	self quit.