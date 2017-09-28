accessing
shouldNotBeRedefined: classSymbol
	(self tooDangerousClasses includes: classSymbol) ifTrue: [ ^true ].
	^Smalltalk globals
		at: classSymbol 
		ifPresent: [ :class | (Smalltalk specialObjectsArray includes: class) or: [class isKindOf: class]]
		ifAbsent: false .
