system compatibility
category: categoryName matches: prefix
	"self default category: 'Compression-Archives' matches: 'Compression'"
	"self default category: 'Compression' matches: 'Compression-Archives'"
	"self default category: 'Compression' matches: 'Compression'"
	"self default category: 'Compression-Arh' matches: 'Compression'"
	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: false) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) = $-