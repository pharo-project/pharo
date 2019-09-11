Class {
	#name : #RGTraitStrategyTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGTraitStrategyTest >> testClassVariablesString [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newTrait.

	self assert: anRGBehavior classVariablesString equals: ''


]

{ #category : #tests }
RGTraitStrategyTest >> testCreationByMethod [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newTrait.
	self assert: (anRGBehavior isBehavior).	
	self assert: (anRGBehavior behaviorStrategy isTraitStrategy).
]

{ #category : #tests }
RGTraitStrategyTest >> testIncompatibleBehavior [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newTrait.
	
	self checkIncompatible: [ 
		anRGBehavior instanceVariables: Array new ].
	self checkIncompatible: [ 
		anRGBehavior kindOfSubclass].
	self checkIncompatible: [ 
		anRGBehavior removeClassVariable: (RGClassVariable named: #ClassVar parent: anRGBehavior)].
	self checkIncompatible: [ 
		anRGBehavior removeSharedPool: (anRGBehavior environment ensureClassNamed: #SomeClass)].
	self checkIncompatible: [ 
		anRGBehavior sharedPoolsDo: [ :sharedPool | ] ].
	self checkIncompatible: [ 
		anRGBehavior defaultMetaClass ].

]

{ #category : #tests }
RGTraitStrategyTest >> testInstSize [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newTrait.
	self assert: anRGBehavior instSize equals: 0
]

{ #category : #tests }
RGTraitStrategyTest >> testInstanceVariablesString [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newTrait.
	self assert: anRGBehavior instanceVariablesString equals: ''
]

{ #category : #tests }
RGTraitStrategyTest >> testKind [

	| strategy |
	
	strategy := RGBehavior newTrait behaviorStrategy.
	self deny: strategy isClassStrategy.
	self deny: strategy isMetaclassStrategy.
	self assert: strategy isTraitStrategy.
	self deny: strategy isMetaclassTraitStrategy.
	
]

{ #category : #tests }
RGTraitStrategyTest >> testResolvingConsistency [

	| anObject |
	anObject := RGTrait unnamed behaviorStrategy.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #tests }
RGTraitStrategyTest >> testSharedPoolNames [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newTrait.
	self assert: (anRGBehavior sharedPoolNames isEmpty).
]

{ #category : #tests }
RGTraitStrategyTest >> testSharedPools [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newTrait.
	self assert: (anRGBehavior sharedPools isEmpty).
]

{ #category : #tests }
RGTraitStrategyTest >> testTrait [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newTrait.
	self assert: anRGBehavior trait equals: anRGBehavior
]
