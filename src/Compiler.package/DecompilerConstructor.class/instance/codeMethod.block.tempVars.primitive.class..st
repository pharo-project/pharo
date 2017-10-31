constructor
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| node methodTemps arguments temporaries |
	node := self codeSelector: selector code: nil.
	tempVars := vars.
	methodTemps := tempVars select: [:t | t scope >= 0].
	arguments := methodTemps copyFrom: 1 to: nArgs.
	temporaries := methodTemps copyFrom: nArgs + 1 to: methodTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	^MethodNode new
		selector: node
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (LegacyEncoder new initScopeAndLiteralTables
					temps: tempVars
					literals: literalValues
					class: class)
		primitive: primitive