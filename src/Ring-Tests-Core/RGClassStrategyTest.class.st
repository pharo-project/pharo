Class {
	#name : #RGClassStrategyTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGClassStrategyTest >> testCategory [

	| aClass |
	
	aClass := RGClass named: #SomeClass.
	aClass category: 'SomeCategory'.
	self assert: aClass category = 'SomeCategory'

]

{ #category : #tests }
RGClassStrategyTest >> testComment [

	| aClass aComment |
	
	aClass := RGClass named: #SomeClass.
	aComment := RGComment parent: aClass.
	aClass comment: aComment.
	
	self assert: (aClass comment == aComment).

]

{ #category : #tests }
RGClassStrategyTest >> testCopying [

	| env class classVariable1 classVariable2 copy |

	class := RGClass unnamed.
	env := class environment.

	classVariable1 := RGClassVariable named: #ClassVariable1 parent: class.
	classVariable2 := RGClassVariable named: #ClassVariable2 parent: class.

	class addClassVariable: classVariable1.
	self assert: class behaviorStrategy classVariables size equals: 1.
	self assert: (class classVariables allSatisfy: [ :each | each parent == class]).

	copy := class copyForBehaviorDefinition.
	class behaviorStrategy classVariables.

	self deny: (copy == class).
	self assert: (copy behaviorStrategy owner == copy).
	self deny: (class behaviorStrategy == copy behaviorStrategy).
	self deny: ((class behaviorStrategy instVarNamed: #classVariables) == (copy behaviorStrategy instVarNamed: #classVariables)).
	self deny: (class behaviorStrategy owner == copy behaviorStrategy owner).
	self assert: (class behaviorStrategy owner == class).
	self assert: (copy behaviorStrategy owner == copy).
	self assert: (class classVariables allSatisfy: [ :each | each parent == class]).
	self assert: class classVariables size equals: copy classVariables size.
	self deny: ((class classVariables first) == (copy classVariables first)).

	self assert: class behaviorStrategy classVariables size equals: 1.

]

{ #category : #tests }
RGClassStrategyTest >> testDefinition [

	| aClass |
	
	aClass := RGClass named: #SomeClass.
	aClass superclass name: #Object.
	
	self assert: (aClass definition beginsWith: 'Object subclass: #SomeClass')	

]

{ #category : #tests }
RGClassStrategyTest >> testDefinitionForNautilus [

	| aClass |
	
	aClass := RGClass named: #SomeClass.
	aClass superclass name: #Object.
	
	self assert: (aClass definitionForNautilus beginsWith: 'Object subclass: #SomeClass')	

]

{ #category : #tests }
RGClassStrategyTest >> testDefinitionWithSlots [

	| aClass slot expression |
	
	aClass := RGClass named: #SomeClass.
	aClass superclass name: #Object.
	slot  := RGUnknownSlot named: #iVar parent: aClass layout.
	aClass layout addSlot: slot.
	expression := 'ToManyRelationSlot inverse: #director inClass: #SlotExampleMovie'.
	slot expression: expression.
	
	self assert: (aClass definition beginsWith: 'Object subclass: #SomeClass').
	self assert: (aClass definition includesSubstring: ('#iVar => ', expression)).

]

{ #category : #tests }
RGClassStrategyTest >> testIncompatibleBehavior [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	
	self checkIncompatible: [ 
		anRGBehavior baseClass: (anRGBehavior environment ensureClassNamed: #SomeOtherClass) ].
	self checkIncompatible: [ 
		anRGBehavior baseTrait ].
	self checkIncompatible: [ 
		anRGBehavior baseTrait: (anRGBehavior environment ensureTraitNamed: #SomeTrait) ].

]

{ #category : #tests }
RGClassStrategyTest >> testKind [

	| strategy |
	
	strategy := RGBehavior newClass behaviorStrategy.
	self assert: strategy isClassStrategy.
	self deny: strategy isMetaclassStrategy.
	self deny: strategy isTraitStrategy.
	self deny: strategy isMetaclassTraitStrategy.
	
]

{ #category : #tests }
RGClassStrategyTest >> testPackage [

	| aClass aPackage |
	
	aClass := RGClass named: #SomeClass.
	aPackage := aClass environment ensurePackageNamed: 'Package'.
	
	aClass package: aPackage.
	self assert: (aClass package == aPackage).
]

{ #category : #tests }
RGClassStrategyTest >> testResolvingConsistency [

	| anObject |
	anObject := RGClass unnamed behaviorStrategy.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #tests }
RGClassStrategyTest >> testStoreString [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	self assert: anRGBehavior storeString equals: '(RGClass named: #unresolved)'.

	anRGBehavior := RGClass named: #SomeClass.
	self assert: anRGBehavior storeString equals: '(RGClass named: #SomeClass)'.	
]
