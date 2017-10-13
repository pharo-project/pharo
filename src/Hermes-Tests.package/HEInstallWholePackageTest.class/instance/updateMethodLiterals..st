utils
updateMethodLiterals: methods
	methods
		do: [ :e | 
			e instVarNamed: #className put: (e className , '2') asSymbol.
			e literals
				do: [ :lit | 
					(lit isKindOf: HEGlobalVariable)
						ifTrue: [ lit instVarNamed: #value put: (lit value , '2') asSymbol ] ] ]