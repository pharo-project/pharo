initialization
initialize: aString
	self initialize.
	packageName := aString asSymbol.
	packages := (RPackage organizer packageNamed: aString ifAbsent: [  nil ])
		ifNotNil: [ :package | { package }  ]
		ifNil: [ {} ]
