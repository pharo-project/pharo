image package loading
category: categoryName matches: prefix

	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: true) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) = $-