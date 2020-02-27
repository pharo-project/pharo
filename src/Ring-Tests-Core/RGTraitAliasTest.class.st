Class {
	#name : #RGTraitAliasTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGTraitAliasTest >> testAliasesCollection [
	
	| traitAlias env alias1 alias2 alias3 |

	traitAlias := RGTraitAlias unnamed.
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
RGTraitAliasTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RGTraitAlias unnamed.
	env := def environment.

	self assert: (def isRingResolved).
		
	self assert: def parent isTraitComposition.
	self assert: def parent isRingResolved not.

	self assert: def parent parent isClass.
	self assert: def parent parent isRingResolved not.
	
	self assert: def parent transformations size equals: 1.

	self checkImplicitSingleClassEnvironmentOf: def parent parent.	

	def2 := RGTraitAlias parent: def parent.
	def parent addTransformation: def2.
	
	self checkImplicitSingleClassEnvironmentOf: def parent parent.	
	
	self assert: def parent transformations size equals: 2.

]

{ #category : #tests }
RGTraitAliasTest >> testNewTraitAlias [

	| traitAlias | 
	traitAlias  := RGTraitAlias unnamed.
	self assert: (traitAlias isRingResolved).
	self assert: (traitAlias hasUnresolvedAll: #(name subject aliases)).
	self assert: (traitAlias isTraitTransformation).
	
]

{ #category : #tests }
RGTraitAliasTest >> testResolvingConsistency [

	| anObject |
	anObject := RGTraitAlias unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #tests }
RGTraitAliasTest >> testUsedTraits [

	| traitAlias trait usedTraits | 
	
	traitAlias := RGTraitAlias unnamed.
	trait := traitAlias environment ensureTraitNamed: #SomeTrait.
	traitAlias subject: trait.
	usedTraits := traitAlias usedTraits.
	
	self assert: usedTraits size equals: 1.
	self assert: usedTraits anyOne equals: trait.
	
	self assert:  traitAlias trait equals: trait.
]
