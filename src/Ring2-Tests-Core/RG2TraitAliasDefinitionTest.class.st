Class {
	#name : #RG2TraitAliasDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2TraitAliasDefinitionTest >> testAliasesCollection [
	
	| traitAlias env alias1 alias2 alias3 |

	traitAlias := RG2TraitAliasDefinition unnamed.
	env := traitAlias environment.

	self assert: traitAlias aliases isEmpty.
	self assert: (traitAlias hasUnresolved: #aliases).
	
	alias1 := (#m2 -> #m1).
	traitAlias addAlias: alias1.
	self assert: traitAlias aliases size equals: 1.

	self assert: (traitAlias hasResolved: #aliases).
	
	alias2 := (#m4 -> #m3).
	traitAlias addAlias: alias2.
	self assert: traitAlias aliases size equals: 2.
	
	alias3 := (#m6 -> #m5).
	traitAlias addAlias: alias3.
	self assert: traitAlias aliases size equals: 3.

	traitAlias removeAlias: alias3.
	self assert: traitAlias aliases size equals: 2.

	traitAlias cleanAliases.
	self assert: traitAlias aliases isEmpty.
	self assert: (traitAlias hasResolved: #aliases).

]

{ #category : #tests }
RG2TraitAliasDefinitionTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RG2TraitAliasDefinition unnamed.
	env := def environment.

	self assert: (def isRing2Resolved).
		
	self assert: def parent isTraitComposition.
	self assert: def parent isRing2Resolved not.

	self assert: def parent parent isClass.
	self assert: def parent parent isRing2Resolved not.
	
	self assert: def parent transformations size equals: 1.

	self checkImplicitSingleClassEnvironmentOf: def parent parent.	

	def2 := RG2TraitAliasDefinition parent: def parent.
	def parent addTransformation: def2.
	
	self checkImplicitSingleClassEnvironmentOf: def parent parent.	
	
	self assert: def parent transformations size equals: 2.

]

{ #category : #tests }
RG2TraitAliasDefinitionTest >> testNewTraitAlias [

	| traitAlias | 
	traitAlias  := RG2TraitAliasDefinition unnamed.
	self assert: (traitAlias isRing2Resolved).
	self assert: (traitAlias hasUnresolvedAll: #(name subject aliases)).
	self assert: (traitAlias isTraitTransformation).
	
]

{ #category : #tests }
RG2TraitAliasDefinitionTest >> testUsedTraits [

	| traitAlias trait usedTraits | 
	
	traitAlias := RG2TraitAliasDefinition unnamed.
	trait := traitAlias environment ensureTraitNamed: #SomeTrait.
	traitAlias subject: trait.
	usedTraits := traitAlias usedTraits.
	
	self assert: usedTraits size equals: 1.
	self assert: usedTraits anyOne equals: trait.
	
	self assert:  traitAlias trait equals: trait.
]
