private
fillOutDefinition: aDictionary
	superclassName := (aDictionary at: '`@superclass')
		ifNotNil: [ :value | value asSymbol ].
	className := (aDictionary at: '`#className') asSymbol.
	instanceVariableNames := self namesIn: (aDictionary
		at: '`#instanceVariableNames'
		ifAbsent: [ String new ]).
	classVariableNames := self namesIn: (aDictionary
		at: '`#classVariableNames'
		ifAbsent: [ String new ]).
	poolDictionaryNames := self namesIn: (aDictionary
		at: '`#poolDictionaries'
		ifAbsent: [ String new ]).
	category := (aDictionary
		at: '`#category'
		ifAbsent: [ #Unclassified ])
			asSymbol