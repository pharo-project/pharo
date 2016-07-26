private
enumerationDictionary
	| decl |
	decl := self enumDecl.
	decl isDictionary
		ifTrue: [ ^ decl ].
	decl := Dictionary newFromPairs: decl.
	^ decl