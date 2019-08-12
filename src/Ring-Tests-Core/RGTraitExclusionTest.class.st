Class {
	#name : #RGTraitExclusionTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGTraitExclusionTest >> testAliasesCollection [
	
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
RGTraitExclusionTest >> testExclusionsCollection [
	
	| traitExclusion env exclusion1 exclusion2 exclusion3 |

	traitExclusion := RGTraitExclusion unnamed.
	env := traitExclusion environment.

	self assert: traitExclusion exclusions isEmpty.
	self assert: (traitExclusion hasUnresolved: #exclusions).
	
	exclusion1 := #m1.
	traitExclusion addExclusion: exclusion1.
	self assert: traitExclusion exclusions size equals: 1.

	self assert: (traitExclusion hasResolved: #exclusions).
	
	exclusion2 := #m2.
	traitExclusion addExclusion: exclusion2.
	self assert: traitExclusion exclusions size equals: 2.
	
	exclusion3 := #m3.
	traitExclusion addExclusion: exclusion3.
	self assert: traitExclusion exclusions size equals: 3.

	traitExclusion removeExclusion: exclusion3.
	self assert: traitExclusion exclusions size equals: 2.

	traitExclusion cleanExclusions.
	self assert: traitExclusion exclusions isEmpty.
	self assert: (traitExclusion hasResolved: #exclusions).

]

{ #category : #tests }
RGTraitExclusionTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RGTraitExclusion unnamed.
	env := def environment.

	self assert: (def isRingResolved).
		
	self assert: def parent isTraitComposition.
	self assert: def parent isRingResolved not.

	self assert: def parent parent isClass.
	self assert: def parent parent isRingResolved not.
	
	self assert: def parent transformations size equals: 1.

	self checkImplicitSingleClassEnvironmentOf: def parent parent.	

	def2 := RGTraitExclusion parent: def parent.
	def parent addTransformation: def2.
	
	self checkImplicitSingleClassEnvironmentOf: def parent parent.	
	
	self assert: def parent transformations size equals: 2.

]

{ #category : #tests }
RGTraitExclusionTest >> testNewTraitAlias [

	| traitAlias | 
	traitAlias  := RGTraitAlias unnamed.
	self assert: (traitAlias isRingResolved).
	self assert: (traitAlias hasUnresolvedAll: #(name subject aliases)).
	self assert: (traitAlias isTraitTransformation).
	
]

{ #category : #tests }
RGTraitExclusionTest >> testNewTraitExlusion [

	| traitExclusion | 
	traitExclusion  := RGTraitExclusion unnamed.
	self assert: (traitExclusion isRingResolved).
	self assert: (traitExclusion hasUnresolvedAll: #(name subject exclusions)).
	self assert: (traitExclusion isTraitTransformation).
	
]

{ #category : #tests }
RGTraitExclusionTest >> testResolvingConsistency [

	| anObject |
	anObject := RGTraitExclusion unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #tests }
RGTraitExclusionTest >> testUsedTraits [

	| traitExclusion trait usedTraits | 
	
	traitExclusion := RGTraitExclusion unnamed.
	trait := traitExclusion environment ensureTraitNamed: #SomeTrait.
	traitExclusion subject: trait.
	usedTraits := traitExclusion usedTraits.
	
	self assert: usedTraits size equals: 1.
	self assert: usedTraits anyOne equals: trait.
]
