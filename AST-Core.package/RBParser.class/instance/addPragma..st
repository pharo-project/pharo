private
addPragma: aPragma
	pragmas ifNil: [ pragmas := OrderedCollection new ].
	pragmas addLast: aPragma