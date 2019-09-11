Class {
	#name : #RGMetaclassStrategyTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGMetaclassStrategyTest >> testCreationByMethod [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newMetaclass.
	self assert: (anRGBehavior isBehavior).	
	self assert: (anRGBehavior behaviorStrategy isMetaclassStrategy).
]

{ #category : #tests }
RGMetaclassStrategyTest >> testIncompatibleBehavior [

	| anRGBehavior aTrait |
	
	anRGBehavior := RGBehavior newMetaclass.
	aTrait := anRGBehavior environment ensureTraitNamed: 'SomeTrait'.
	
	self checkIncompatible: [ 
		anRGBehavior addClassVariable: (RGClassVariable named: #classVar parent: anRGBehavior) ].
	self checkIncompatible: [ 
		anRGBehavior allClassVariables ].
	self checkIncompatible: [ 
		anRGBehavior classVariableDefinitionString ].
	self checkIncompatible: [ 
		anRGBehavior classVariables ].
	self checkIncompatible: [ 
		anRGBehavior classVariables: Array new ].
	self checkIncompatible: [ 
		anRGBehavior classVariablesDo: [:classVar | ] ].
	self checkIncompatible: [ 
		anRGBehavior cleanClassVariables ].
	
	self checkIncompatible: [ 
		anRGBehavior addSharedPool: (anRGBehavior environment ensureClassNamed: #SomePool) ].
	self checkIncompatible: [ 
		anRGBehavior cleanSharedPools ].

	self checkIncompatible: [ 
		anRGBehavior category: #someCategory ].

	self checkIncompatible: [ 
		anRGBehavior classTrait ].

	self checkIncompatible: [ 
		anRGBehavior classTrait: aTrait classTrait ].

	self checkIncompatible: [ 
		anRGBehavior comment: (RGComment parent: anRGBehavior) ].

	self checkIncompatible: [ 
		anRGBehavior sharedPools: Array new ].

	self checkIncompatible: [ 
		anRGBehavior trait ].


]

{ #category : #tests }
RGMetaclassStrategyTest >> testKind [

	| strategy |
	
	strategy := RGBehavior newMetaclass behaviorStrategy.
	self deny: strategy isClassStrategy.
	self assert: strategy isMetaclassStrategy.
	self deny: strategy isTraitStrategy.
	self deny: strategy isMetaclassTraitStrategy.
	
]

{ #category : #tests }
RGMetaclassStrategyTest >> testResolvingConsistency [

	| anObject |
	anObject := RGMetaclass unnamed behaviorStrategy.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #tests }
RGMetaclassStrategyTest >> testSetBaseClass [

	| env class1 class2 |
	
	env := RGEnvironment new.
	class1 := env ensureClassNamed: 'SomeClass1'.
	class2 := env ensureClassNamed: 'SomeClass2'.
	self assert: class1 metaclass baseBehavior equals: class1.
	class1 metaclass baseClass: class2.
	self assert: class1 metaclass baseBehavior equals: class2.
	
]

{ #category : #tests }
RGMetaclassStrategyTest >> testSharedPools [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newMetaclass.
	self assert: (anRGBehavior sharedPools) isEmpty.
]

{ #category : #tests }
RGMetaclassStrategyTest >> testStoreString [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newMetaclass.
	self assert: anRGBehavior storeString equals: '(RGMetaclass named: #unresolved)'.

	anRGBehavior := RGMetaclass named: #SomeMetaclass.
	self assert: anRGBehavior storeString equals: '(RGMetaclass named: #SomeMetaclass)'.	
]

{ #category : #tests }
RGMetaclassStrategyTest >> testTheMetaclass [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newMetaclass.
	self assert: (anRGBehavior theMetaClass) equals: anRGBehavior.
]
