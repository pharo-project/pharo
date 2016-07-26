argument loaders
loaderFromMethodArgsNamed: argName
	| index |

	methodArgs ifNil: [ ^ nil ].
	index := methodArgs indexOf: argName ifAbsent: [ nil ].
	^ index ifNotNil: [ 
		"ok, this is a method argument"
		^ FFIMethodArgument new 
			argName: argName;
			index: index;
			yourself ].
