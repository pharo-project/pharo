query
instanceSideMethodsWithNilKeyInLastLiteral
	"This answers all the instance side methods that has NIL as the key in their last literal. There should be none (only class side methods have this)"

	^ self
		allMethodsSelect: [ :each | 
			(((each literalAt: each numLiterals) key isNil and: [ (each literalAt: each numLiterals) value isMeta not ])
				and: [ each methodClass ~= UndefinedObject ]) and: [ each isInstalled ] ]