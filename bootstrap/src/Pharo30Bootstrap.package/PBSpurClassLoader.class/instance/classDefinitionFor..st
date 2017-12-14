as yet unclassified
classDefinitionFor: aClass
	"Answer a String that defines the receiver."

	| type |
	type := aClass isWeak
						ifTrue: [ 'WeakLayout' ]
						ifFalse: [ aClass isPointers
									ifTrue: [ aClass isVariable
												ifTrue: [ 'VariableLayout' ]
												ifFalse: [ 'FixedLayout' ]
											]
									ifFalse: [  aClass isCompiledMethod
										ifTrue: [ 'CompiledMethodLayout' ]
										ifFalse: [ aClass isWords
											ifTrue: [ 'WordLayout' ]
											ifFalse: [ 'ByteLayout' ]]]].
	^ '| newClass |
	newClass := ShiftClassInstaller make: [ :builder |
		builder
			superclass: {superClass};
			name: ''{name}'';
			layoutClass: {type};
			slots: ''{instanceVariablesString}'' asSlotCollection;
			sharedVariablesFromString: ''{classVariablesString}'';
			sharedPools: ''{sharedPoolsString}'';
			category: ''{category}'';
			environment: {superClass} environment;
			classSlots: ''{classInstanceVariablesString}'' asSlotCollection ].
	newClass setTraitComposition: {aTraitComposition} asTraitComposition.
	newClass'
		format: { 
			'superClass' -> (aClass superclass ifNil: ['nil'] ifNotNil: [ :superclass | superclass name ]).
			'name' -> aClass name.
			'type' -> type.
			'instanceVariablesString' -> (' ' join: aClass instVarNames).
			'classVariablesString' -> aClass classVariablesString.
			'sharedPoolsString' -> aClass sharedPoolsString.
			'category' -> aClass category asString.
			'classInstanceVariablesString' -> (' ' join: aClass classSide instVarNames).
			'aTraitComposition' -> aClass traitCompositionString } asDictionary.