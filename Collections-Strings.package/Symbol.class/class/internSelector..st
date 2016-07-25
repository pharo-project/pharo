instance creation
internSelector: aStringOrSymbol
	| selector |
	selector := (self selectorTable like: aStringOrSymbol)
		ifNil: [ self selectorTable add: aStringOrSymbol asSymbol ].
	^ selector