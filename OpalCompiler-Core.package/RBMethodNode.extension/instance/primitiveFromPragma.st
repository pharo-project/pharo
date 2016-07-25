*OpalCompiler-Core
primitiveFromPragma
	pragmas ifNil: [ ^ IRPrimitive null ].
	^ pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive asPrimitive ]
		ifNone: [ IRPrimitive null ]