accessing
versionString: aStringOrSymbol

	aStringOrSymbol isSymbol ifFalse: [ self error: 'Version symbol ', aStringOrSymbol printString, ' for symbolic version method must be a Symbol' ].
	super versionString: aStringOrSymbol