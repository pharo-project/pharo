Extension { #name : #ShiftClassBuilder }

{ #category : #'*ClassParser' }
ShiftClassBuilder >> buildFromAST: aCDClassDefinitionNode [

	| package |

	self name: aCDClassDefinitionNode className.
	
	aCDClassDefinitionNode superclassName = #nil
		ifFalse: [ self
				superclass:
					(superclassResolver
						resolve: aCDClassDefinitionNode superclassName
						inEnv: buildEnvironment) ]
		ifTrue: [ self superclass: nil ].
		
	self layoutClass: FixedLayout.
	aCDClassDefinitionNode isEphemeron
		ifTrue: [ self layoutClass: EphemeronLayout ].
	aCDClassDefinitionNode isWeak
		ifTrue: [ self layoutClass: WeakLayout ].
	aCDClassDefinitionNode isImmediate
		ifTrue: [ self layoutClass: ImmediateLayout ].
	aCDClassDefinitionNode isVariableClass
		ifTrue: [ self layoutClass: VariableLayout ].
	aCDClassDefinitionNode isWords
		ifTrue: [ self layoutClass: WordLayout ].
	aCDClassDefinitionNode isBytes
		ifTrue: [ self layoutClass: ByteLayout ].
		
	self slots: (aCDClassDefinitionNode slots collect: [ :e | e asSlot ]).
	
	self sharedVariables: (aCDClassDefinitionNode sharedSlots collect: [ :e | e ]).
	
	aCDClassDefinitionNode packageName
		ifNotNil: [ :cat | package := cat ]
		ifNil: [ package := 'Unclassified' ].
	self category: package.
	
	self layoutDefinition sharedPools: (aCDClassDefinitionNode sharedPools collect: [ :e | e name ]).
	
	aCDClassDefinitionNode traitDefinition
		ifNotNil: [ :traitDef | 
			traitDef originalNode formattedCode.
			self
				traitComposition:
					(self class compiler
						source: traitDef originalNode formattedCode;
							evaluate) ]
]
