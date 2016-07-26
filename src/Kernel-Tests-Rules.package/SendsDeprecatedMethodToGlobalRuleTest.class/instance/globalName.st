properties
globalName

	^ globalName ifNil: [ 
		[ globalName :=
			('TestGlobal', 1e10 atRandom asString) asSymbol
		] doWhileTrue: [ 
			Smalltalk globals includesKey: globalName ] ]