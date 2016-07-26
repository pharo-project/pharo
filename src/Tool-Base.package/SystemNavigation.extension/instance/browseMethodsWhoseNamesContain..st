*Tool-Base
browseMethodsWhoseNamesContain: aString
	"Launch a tool which shows all methods whose names contain the given 	string; case-insensitive."

	^ self browseAllSelect: [ :e | e selector includesSubstring: aString caseSensitive: false ].