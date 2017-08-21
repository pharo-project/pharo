class-loading
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
	newClass := PharoClassInstaller make: [ :builder |
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
	newClass layout compactClassIndex: {compactIndex}.
	newClass'
		format: { 
			'superClass' -> aClass superclass name.
			'name' -> aClass name.
			'type' -> type.
			'instanceVariablesString' -> (' ' join: aClass instVarNames).
			'classVariablesString' -> aClass classVariablesString.
			'sharedPoolsString' -> aClass sharedPoolsString.
			'category' -> aClass category asString.
			'classInstanceVariablesString' -> (' ' join: aClass classSide instVarNames).
			'compactIndex' -> (self compactClassIndexForClass: aClass) } asDictionary.