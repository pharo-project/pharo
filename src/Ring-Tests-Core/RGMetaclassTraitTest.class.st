Class {
	#name : #RGMetaclassTraitTest,
	#superclass : #RGTest,
	#traits : 'TRGBehaviorTest',
	#classTraits : 'TRGBehaviorTest classTrait',
	#category : #'Ring-Tests-Core'
}

{ #category : #'as yet unclassified' }
RGMetaclassTraitTest >> behaviorClass [

	^ RGMetaclassTrait
]

{ #category : #'as yet unclassified' }
RGMetaclassTraitTest >> testEnsureProtocol [

	| behavior |
	
	behavior := self behaviorClass named: #SomeClass.
	behavior ensureProtocolNamed: #SomeProtocol.
	self assert: behavior protocols size equals: 1.
	self assert: behavior protocols first equals: #SomeProtocol.
	
]

{ #category : #tests }
RGMetaclassTraitTest >> testImplicitConversionFromClass [

	| env |
	
	env := RGEnvironment new.
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
RGMetaclassTraitTest >> testImplicitConversionFromClassProperties [

	| env entity |
	
	env := RGEnvironment new.
	entity := env ensureClassNamed: #SomeTrait.	
	entity := env ensureTraitNamed: #'SomeTrait'.
	self assert: entity classTrait isTrait.
	self assert: entity classTrait baseTrait equals: entity.

]

{ #category : #tests }
RGMetaclassTraitTest >> testMetaclassTraitImplicitEnvironment [

	| trait |
	
	trait := RGMetaclassTrait unnamed.
	self assert: (trait hasUnresolved: #name).
	self checkImplicitSingleTraitEnvironmentOf: trait baseTrait.
	
	
	
	

]

{ #category : #tests }
RGMetaclassTraitTest >> testNewMetaclassTrait [
	| trait baseTrait |
	trait := RGMetaclassTrait unnamed.
	self assert: trait isRingResolved.
	self assert: (trait hasUnresolved: #baseTrait).
	self assert: trait isTrait.

	baseTrait := RGTrait parent: trait environment.
	trait baseTrait: baseTrait.
	self assert: (trait hasResolved: #baseTrait).
	self assert: trait baseTrait identicalTo: baseTrait
]
