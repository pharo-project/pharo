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

	self layoutClass: aCDClassDefinitionNode layoutClass.

	self slots: (aCDClassDefinitionNode slots collect: [ :e | e asSlot]).

	self sharedVariables: (aCDClassDefinitionNode sharedVariables collect: [ :e | e asClassVariable]).

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
