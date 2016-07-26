pragmas
pragmaPrimitives
	| primitiveSelectors primitives |
	properties isEmpty ifTrue:
		[^0].
	primitiveSelectors := self class primitivePragmaSelectors.
	primitives := properties pragmas select:
					[:pragma|
					primitiveSelectors includes: pragma keyword].
	primitives isEmpty ifTrue:
		[^0].
	primitives size > 1 ifTrue:
		[^self notify: 'Ambigous primitives'].
	^self perform: primitives first keyword withArguments: primitives first arguments