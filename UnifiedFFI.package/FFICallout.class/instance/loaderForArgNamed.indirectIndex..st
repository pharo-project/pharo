argument loaders
loaderForArgNamed: argName indirectIndex: anIndex
	| loader |
	loader := self loaderForArgNamed: argName.
	anIndex ifNotNil: [ loader := self indirectLoader: loader byIndex: anIndex ].
	^ loader