Class {
	#name : #RG2ResolveBehaviorInformationTest,
	#superclass : #RG2Test,
	#instVars : [
		'env'
	],
	#category : #'Ring2-Tests-Core'
}

{ #category : #support }
RG2ResolveBehaviorInformationTest >> detachFromImage [

	env backend: (RG2EnvironmentBackend new).
]

{ #category : #running }
RG2ResolveBehaviorInformationTest >> setUp [

	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testClassComment [

	| comment realClass |
	
	realClass := RG2TestClassE.
	comment := realClass asRing2MinimalCommentDefinitionIn: env.
	comment resolveBehaviorInformation.
	self detachFromImage.	
	self assert: comment content = realClass comment.	
	self assert: comment stamp equals: realClass organization commentStamp
]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testClassVariable [

	| var realVar |
	
	realVar := RG2TestClassE classVariableNamed: #classVar1.
	var := realVar asRing2MinimalDefinitionIn: env.
	var resolveBehaviorInformation.
	self detachFromImage.	
	self assert: var name equals: #classVar1.

]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testInstanceVariableSlotDefinition [

	| slot realSlot |
	
	realSlot := RG2TestClassE slotNamed: #var1.
	slot := realSlot asRing2MinimalDefinitionIn: env.
	slot resolveBehaviorInformation.
	self detachFromImage.	
	self assert: slot name equals: #var1.

]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testLayout [

	| layout realLayout |
	
	realLayout := RG2TestClassE classLayout.
	layout := realLayout asRing2MinimalDefinitionIn: env.
	layout resolveBehaviorInformation.
	self detachFromImage.	
	self assert: layout slots size equals: 2.
	self assert: layout slots first name equals: #var1.
	self assert: layout slots second name equals: #var2.
]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testPackage [

	| package realPackage packageName |
	
	packageName := #'Ring2-Tests-Core'.
	realPackage := packageName asPackage.
	package := realPackage asRing2MinimalDefinitionIn: env.
	package resolveBehaviorInformation.
	self detachFromImage.	
	self assert: package name equals: packageName.
]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testSharedPoolVariable [

	| var realClass |
	
	realClass := RG2TestPoolUser.
	var := (realClass asRing2MinimalDefinitionIn: env) sharedPools first.
	var resolveBehaviorInformation.
	self detachFromImage.	
	self assert: var name equals: #RG2TestPool.

]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testTraitAlias [

	| alias realAlias |
	
	realAlias := RG2TestClassD traitComposition transformations first.
	alias := realAlias asRing2MinimalDefinitionIn: env.
	alias resolveBehaviorInformation.
	self detachFromImage.	
	self assert: alias subject name equals: #RG2TestTraitB.	
	self assert: alias subject isTrait.
	self assert: alias subject isRing2Object.
	self assertCollection: (alias aliases ) hasSameElements: { #c3->#c2 }.

]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testTraitComposition [

	| composition realComposition transformation1 transformation2 |
	
	realComposition := RG2TestClassB traitComposition.
	composition := realComposition asRing2MinimalDefinitionIn: env.
	composition resolveBehaviorInformation.
	self detachFromImage.	
	self assert: composition transformations size equals: 2.	
	transformation1 := composition transformations first.
	transformation2 := composition transformations second.
	self assert: transformation1 isTrait.
	self assert: transformation1 isRing2Object.
	self assert: transformation1 name equals: #RG2TestTraitA.
	self assert: transformation2 isTraitExclusion.
	self assert: transformation2 subject name equals: #RG2TestTraitB.
	self assertCollection: transformation2 exclusions hasSameElements: { #c }.
]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testTraitExclusions [

	| exclusion realExclusion |
	
	realExclusion := RG2TestClassB traitComposition transformations second.
	exclusion := realExclusion asRing2MinimalDefinitionIn: env.
	exclusion resolveBehaviorInformation.
	self detachFromImage.	
	self assert: exclusion subject name equals: #RG2TestTraitB.	
	self assert: exclusion subject isTrait.
	self assert: exclusion subject isRing2Object.
	self assertCollection: (exclusion exclusions) hasSameElements: { #c }.

]

{ #category : #tests }
RG2ResolveBehaviorInformationTest >> testUnknownSlot [

	| slot realSlot |
	
	realSlot := RG2TestSlotUser slotNamed: #unknown.
	slot := realSlot asRing2MinimalDefinitionIn: env.
	slot resolveBehaviorInformation.
	self detachFromImage.	
	self assert: slot name equals: #unknown.
	self assert: slot expression equals: 'RG2TestSlot forTesting' 

]
