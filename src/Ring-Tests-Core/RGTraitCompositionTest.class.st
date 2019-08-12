Class {
	#name : #RGTraitCompositionTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGTraitCompositionTest >> testGlobalVariablesCollection [
	
	| globalVariable globalVariable2 env |

	env := RGEnvironment new.

	self assert: env ask globalVariables isEmpty.
	self assert: (env hasUnresolved: #globalVariables).
	
	globalVariable := RGGlobalVariable named: #SomeGlobal parent: env.
	env addGlobalVariable: globalVariable.
	self assert: env ask globalVariables size equals: 1.

	self assert: (env hasResolved: #globalVariables).
	
	globalVariable2 := RGGlobalVariable named: #SomeGlobal2 parent: env.
	env addGlobalVariable: globalVariable2.
	self assert: env ask globalVariables size equals: 2.
	
	env removeGlobalVariable: globalVariable2.
	self assert: env ask globalVariables size equals: 1.

	env cleanGlobalVariables.
	self assert: env ask globalVariables isEmpty.
	self assert: (env hasResolved: #globalVariables).

]

{ #category : #tests }
RGTraitCompositionTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RGTraitComposition unnamed.
	env := def environment.

	self checkImplicitSingleClassEnvironmentOf: def parent.	

	self assert: (def isRingResolved).
	
	def2 := RGTraitComposition parent: def parent .
	self checkImplicitSingleClassEnvironmentOf: def parent.	

]

{ #category : #tests }
RGTraitCompositionTest >> testNewTraitComposition [

	| traitComposition | 
	traitComposition  := RGTraitComposition unnamed.
	self assert: (traitComposition isRingResolved).
	self assert: (traitComposition hasUnresolvedAll: #(name transformations)).
	self assert: (traitComposition isTraitComposition).
	
]

{ #category : #tests }
RGTraitCompositionTest >> testResolvingConsistency [

	| anObject |
	anObject := RGTraitComposition unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #tests }
RGTraitCompositionTest >> testTransformationsCollection [
	
	| traitComposition env transformation1 transformation2 transformation3 |

	traitComposition := RGTraitComposition unnamed.
	env := traitComposition environment.

	self assert: traitComposition transformations isEmpty.
	self assert: (traitComposition hasUnresolved: #transformations).
	
	transformation1 := RGTraitComposition parent: env.
	env addBehavior: transformation1.
	traitComposition addTransformation: transformation1.
	self assert: traitComposition transformations size equals: 1.

	self assert: (traitComposition hasResolved: #transformations).
	
	transformation2 := RGTraitAlias parent: traitComposition.
	traitComposition addTransformation: transformation2.
	self assert: traitComposition transformations size equals: 2.
	
	transformation3 := RGTraitExclusion parent: traitComposition.
	traitComposition addTransformation: transformation3.
	self assert: traitComposition transformations size equals: 3.

	traitComposition removeTransformation: transformation3.
	self assert: traitComposition transformations size equals: 2.

	traitComposition cleanTransformations.
	self assert: traitComposition transformations isEmpty.
	self assert: (traitComposition hasResolved: #transformations).

]

{ #category : #tests }
RGTraitCompositionTest >> testUsedTraits [
	| traitComposition env trait1 trait2 trait3 trait4 transformation2 transformation3 usedTraits |
	traitComposition := RGTraitComposition unnamed.
	env := traitComposition environment.
	trait1 := env ensureTraitNamed: #SomeTrait1.
	trait2 := env ensureTraitNamed: #SomeTrait2.
	trait3 := env ensureTraitNamed: #SomeTrait3.
	trait4 := env ensureTraitNamed: #SomeTrait4.
	traitComposition addTransformation: trait1.
	transformation2 := RGTraitAlias parent: traitComposition.
	transformation2 subject: trait2.
	traitComposition addTransformation: transformation2.
	self assert: traitComposition transformations size equals: 2.
	transformation3 := RGTraitExclusion
		parent: traitComposition.
	transformation3 subject: trait3.
	traitComposition addTransformation: transformation3.
	self assert: traitComposition transformations size equals: 3.
	usedTraits := traitComposition usedTraits.
	self assert: usedTraits size equals: 3.
	self
		assert:
			(usedTraits
				includesAll:
					{trait1.
					trait2.
					trait3})
]
