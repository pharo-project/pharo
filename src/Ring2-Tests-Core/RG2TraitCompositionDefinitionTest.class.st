Class {
	#name : #RG2TraitCompositionDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2TraitCompositionDefinitionTest >> testGlobalVariablesCollection [
	
	| globalVariable globalVariable2 traitComposition env transformation1 transformation2 transformation3 |

	env := RG2Environment new.

	self assert: env ask globalVariables isEmpty.
	self assert: (env hasUnresolved: #globalVariables).
	
	globalVariable := RG2GlobalVariableDefinition named: #SomeGlobal parent: env.
	env addGlobalVariable: globalVariable.
	self assert: env ask globalVariables size equals: 1.

	self assert: (env hasResolved: #globalVariables).
	
	globalVariable2 := RG2GlobalVariableDefinition named: #SomeGlobal2 parent: env.
	env addGlobalVariable: globalVariable2.
	self assert: env ask globalVariables size equals: 2.
	
	env removeGlobalVariable: globalVariable2.
	self assert: env ask globalVariables size equals: 1.

	env cleanGlobalVariables.
	self assert: env ask globalVariables isEmpty.
	self assert: (env hasResolved: #globalVariables).

]

{ #category : #tests }
RG2TraitCompositionDefinitionTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RG2TraitCompositionDefinition unnamed.
	env := def environment.

	self checkImplicitSingleClassEnvironmentOf: def parent.	

	self assert: (def isRing2Resolved).
	
	def2 := RG2TraitCompositionDefinition parent: def parent .
	self checkImplicitSingleClassEnvironmentOf: def parent.	

]

{ #category : #tests }
RG2TraitCompositionDefinitionTest >> testNewTraitComposition [

	| traitComposition | 
	traitComposition  := RG2TraitCompositionDefinition unnamed.
	self assert: (traitComposition isRing2Resolved).
	self assert: (traitComposition hasUnresolvedAll: #(name transformations)).
	self assert: (traitComposition isTraitComposition).
	
]

{ #category : #tests }
RG2TraitCompositionDefinitionTest >> testTransformationsCollection [
	
	| traitComposition env transformation1 transformation2 transformation3 |

	traitComposition := RG2TraitCompositionDefinition unnamed.
	env := traitComposition environment.

	self assert: traitComposition transformations isEmpty.
	self assert: (traitComposition hasUnresolved: #transformations).
	
	transformation1 := RG2TraitCompositionDefinition parent: env.
	env addBehavior: transformation1.
	traitComposition addTransformation: transformation1.
	self assert: traitComposition transformations size equals: 1.

	self assert: (traitComposition hasResolved: #transformations).
	
	transformation2 := RG2TraitAliasDefinition parent: traitComposition.
	traitComposition addTransformation: transformation2.
	self assert: traitComposition transformations size equals: 2.
	
	transformation3 := RG2TraitExclusionDefinition parent: traitComposition.
	traitComposition addTransformation: transformation3.
	self assert: traitComposition transformations size equals: 3.

	traitComposition removeTransformation: transformation3.
	self assert: traitComposition transformations size equals: 2.

	traitComposition cleanTransformations.
	self assert: traitComposition transformations isEmpty.
	self assert: (traitComposition hasResolved: #transformations).

]

{ #category : #tests }
RG2TraitCompositionDefinitionTest >> testUsedTraits [
	| traitComposition env trait1 trait2 trait3 trait4 transformation2 transformation3 usedTraits |
	traitComposition := RG2TraitCompositionDefinition unnamed.
	env := traitComposition environment.
	trait1 := env ensureTraitNamed: #SomeTrait1.
	trait2 := env ensureTraitNamed: #SomeTrait2.
	trait3 := env ensureTraitNamed: #SomeTrait3.
	trait4 := env ensureTraitNamed: #SomeTrait4.
	traitComposition addTransformation: trait1.
	transformation2 := RG2TraitAliasDefinition parent: traitComposition.
	transformation2 subject: trait2.
	traitComposition addTransformation: transformation2.
	self assert: traitComposition transformations size equals: 2.
	transformation3 := RG2TraitExclusionDefinition
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
