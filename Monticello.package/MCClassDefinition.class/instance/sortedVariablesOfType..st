installing
sortedVariablesOfType: aSymbol
	"version for complex vars, { definition . definition }"
	^(((self selectVariables: aSymbol) asSortedCollection collect: [:each | Smalltalk compiler evaluate: each])) asArray.
		