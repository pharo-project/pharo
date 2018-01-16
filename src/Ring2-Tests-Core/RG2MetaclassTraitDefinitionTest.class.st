Class {
	#name : #RG2MetaclassTraitDefinitionTest,
	#superclass : #RG2Test,
	#traits : 'TRG2BehaviorDefinitionTest',
	#classTraits : 'TRG2BehaviorDefinitionTest classTrait',
	#category : #'Ring2-Tests-Core'
}

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinitionTest >> behaviorClass [

	^ RG2MetaclassTraitDefinition
]

{ #category : #'as yet unclassified' }
RG2MetaclassTraitDefinitionTest >> testEnsureProtocol [

	| behavior protocol |
	
	behavior := self behaviorClass named: #SomeClass.
	behavior ensureProtocolNamed: #SomeProtocol.
	self assert: behavior protocols size equals: 1.
	self assert: behavior protocols first equals: #SomeProtocol.
	
]

{ #category : #tests }
RG2MetaclassTraitDefinitionTest >> testImplicitConversionFromClass [

	| env |
	
	env := RG2Environment new.
	env ensureClassNamed: #SomeTrait.
	self assert: (env ask behaviors select: [ :e | e name = #'SomeTrait' ]) size equals: 1.
	self assert: (env ask behaviors select: [ :e | e name = #'SomeTrait clss' ]) size equals: 0.
	
	env ensureTraitNamed: #'SomeTrait'.
	self assert: (env ask behaviors select: [ :e | e name = #'SomeTrait' ]) size equals: 1.
	self assert: (env ask behaviors select: [ :e | e name = #'SomeTrait classTrait' ]) size equals: 0.
	self assert: (env ask behaviors select: [ :e | e name = #'SomeTrait clss' ]) size equals: 0.
	env.

]

{ #category : #tests }
RG2MetaclassTraitDefinitionTest >> testMetaclassTraitImplicitEnvironment [

	| trait |
	
	trait := RG2MetaclassTraitDefinition unnamed.
	self assert: (trait hasUnresolved: #name).
	self checkImplicitSingleTraitEnvironmentOf: trait baseTrait.
	
	
	
	

]

{ #category : #tests }
RG2MetaclassTraitDefinitionTest >> testMoveExtensionMethodsBetweenPackages [
	
	| env aClass package1 package2 package3 method |
	
	env := RG2Environment new.
	package1 := env ensurePackageNamed: #Package1.
	package2 := env ensurePackageNamed: #Package2.
	package3 := env ensurePackageNamed: #Package3.
	aClass := env ensureClassNamed: #SomeClass.
	method := aClass ensureLocalMethodNamed: 'msg'.
	aClass package: package1.
	method package: package2.
	self assert: aClass package equals: package1.
	self assert: method package equals: package2.
	self assert: (package1 extensionMethods isEmpty).
	self assert: (package2 extensionMethods includes: method).
	self assert: (package3 extensionMethods isEmpty).

	method package: package3.
	
	self assert: aClass package equals: package1.
	self assert: method package equals: package3.
	self assert: (package1 extensionMethods isEmpty).
	self assert: (package2 extensionMethods isEmpty).
	self assert: (package3 extensionMethods includes: method).
	


]

{ #category : #tests }
RG2MetaclassTraitDefinitionTest >> testNewMetaclassTrait [

	| trait baseTrait | 
	trait  := RG2MetaclassTraitDefinition unnamed.
	self assert: (trait isRing2Resolved).
	self assert: (trait hasUnresolved: #baseTrait).
	self assert: (trait isTrait).
	
	baseTrait  := RG2TraitDefinition parent: trait environment.
	trait baseTrait: baseTrait.
	self assert: (trait hasResolved: #baseTrait).
	self assert: (trait baseTrait == baseTrait).

]
