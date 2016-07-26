argument loaders
loaderForArgNamed: argName
	| loader |
	"try getting the argument from the method arguments"
	loader := self loaderFromMethodArgsNamed: argName.
	loader ifNil: [ 
		"special case, receiver argument"
		argName = 'self' ifTrue: [ loader := self receiverArgumentLoader ].
		loader ifNil: [ 
			"Ask the requestor for the argument"
			loader := requestor ffiInstVarArgument: argName generator: self.
			loader 
				ifNil: [ Error signal: 'Could not find accessor for variable named "' , argName , '"' ] ] ].
	^ loader