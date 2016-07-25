installing
variablesOfType: aSymbol
	"version for complex vars, { definition . definition }"
	^(((self selectVariables: aSymbol) collect: [:each | Smalltalk compiler evaluate: each])) asArray.
		