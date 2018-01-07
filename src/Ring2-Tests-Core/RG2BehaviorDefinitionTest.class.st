Class {
	#name : #RG2BehaviorDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2BehaviorDefinitionTest >> behaviorClass [

	^ RG2BehaviorDefinition
]

{ #category : #tests }
RG2BehaviorDefinitionTest >> testBehaviorWithMethodTags [
	| newBehavior |
	
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: (newBehavior name = #SomeClass).
	self assert: (newBehavior hasUnresolvedAll: #(superclass localMethods protocols)).
	self assert: (newBehavior methods isEmpty).

	1 to: 10 do: [ :i |
		newBehavior addMethodTag: ('tag', i asString) asSymbol  ].
	
	self assert: (newBehavior hasResolved: #tagsForMethods).

	self assert: (newBehavior tagsForMethods allSatisfy: #isSymbol).
	
	self assert: (newBehavior tagsForMethods size = 10).
	(newBehavior tagsForMethods first: 4) do: [:each | newBehavior removeMethodTag: each ].
	self assert: (newBehavior tagsForMethods size = 6).
	newBehavior cleanTagsForMethods.
	self assert: (newBehavior tagsForMethods isEmpty).

]

{ #category : #tests }
RG2BehaviorDefinitionTest >> testBehaviorWithProtocols [
	| newBehavior |
	
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: (newBehavior name = #SomeClass).
	self assert: (newBehavior hasUnresolvedAll: #(superclass localMethods protocols)).
	self assert: (newBehavior methods isEmpty).

	1 to: 10 do: [ :i |
		newBehavior addMethodTag: ('tag', i asString) asSymbol  ].
	
	self assert: (newBehavior protocols allSatisfy: #isSymbol).
	
	self assert: (newBehavior protocols size = 10).
	(newBehavior protocols first: 4) do: [:each | newBehavior removeProtocol: each ].
	self assert: (newBehavior protocols size = 6).
	newBehavior cleanProtocols.
	self assert: (newBehavior protocols isEmpty).

]

{ #category : #tests }
RG2BehaviorDefinitionTest >> testSiblings [
	| newBehavior |
	
	newBehavior := RG2ClassDefinition named: #SomeBehaivor.
	self assert: newBehavior sibling equals: newBehavior metaclass.
	self assert: newBehavior baseBehavior equals: newBehavior.
	self assert: newBehavior instanceSide equals: newBehavior.
	self assert: newBehavior theNonMetaClass equals: newBehavior.
	
	newBehavior := RG2MetaclassDefinition named: #SomeBehaivor.
	self assert: newBehavior sibling equals: newBehavior baseClass.
	self assert: newBehavior baseBehavior equals: newBehavior baseClass.
	self assert: newBehavior instanceSide equals: newBehavior baseClass.
	self assert: newBehavior theNonMetaClass equals: newBehavior baseClass.

	newBehavior := RG2TraitDefinition named: #SomeBehaivor.
	self assert: newBehavior sibling equals: newBehavior classTrait.
	self assert: newBehavior baseBehavior equals: newBehavior.
	self assert: newBehavior instanceSide equals: newBehavior.
	self assert: newBehavior theNonMetaClass equals: newBehavior.

	newBehavior := RG2MetaclassTraitDefinition named: #SomeBehaivor.
	self assert: newBehavior sibling equals: newBehavior baseTrait.
	self assert: newBehavior baseBehavior equals: newBehavior baseTrait.
	self assert: newBehavior instanceSide equals: newBehavior baseTrait.
	self assert: newBehavior theNonMetaClass equals: newBehavior baseTrait.


]
