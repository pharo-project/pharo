argument loaders
loaderFromMethodArgsNamed: argName
	(argName beginsWith: 'var')
		ifTrue: [ ^ 
			FFIMethodArgument new  
				argName: argName;
				index: (argName allButFirst: 3) asInteger ].
	^ nil