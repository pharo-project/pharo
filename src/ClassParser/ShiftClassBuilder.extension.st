Extension { #name : 'ShiftClassBuilder' }

{ #category : '*ClassParser' }
ShiftClassBuilder >> buildFromAST: aCDClassDefinitionNode [

	self name: aCDClassDefinitionNode className.

	self superclass: (aCDClassDefinitionNode superclassName = #nil
			 ifTrue: [ nil ]
			 ifFalse: [ superclassResolver resolve: aCDClassDefinitionNode superclassName inEnv: buildEnvironment ]).

	self layoutClass: aCDClassDefinitionNode layoutClass.

	self slots: (aCDClassDefinitionNode slots collect: [ :e | e asSlot ]).

	self sharedVariables: (aCDClassDefinitionNode sharedVariables collect: [ :e | e asClassVariable ]).

	aCDClassDefinitionNode packageName ifNotNil: [ :aPackage | self package: aPackage ].

	aCDClassDefinitionNode tag ifNotNil: [ :aTag | self tag: aTag ].

	self sharedPools: (aCDClassDefinitionNode sharedPools collect: [ :e | e name ]).

	aCDClassDefinitionNode traitDefinition ifNotNil: [ :traitDef | self traitComposition: traitDef originalNode evaluate ]
]
