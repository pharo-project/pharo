private-formatting
formatPragmasFor: aMethodNode
	aMethodNode pragmas do: [ :each | self visitNode: each; newLine ]