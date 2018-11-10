Class {
	#name : #ClyInheritedTraitScopeTests,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemPlugins-Traits-Queries-Tests'
}

{ #category : #running }
ClyInheritedTraitScopeTests >> scopeClass [
	
	^ClyInheritedTraitScope
]

{ #category : #tests }
ClyInheritedTraitScopeTests >> testClassEnumeration [
	| expected |
	scope := ClyInheritedTraitScope of: ClyClassWithTraits.
	
	scope classesDo: [ :each | result add: each ].
	
	expected := ClyClassWithTraits traitComposition allTraits.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClyInheritedTraitScopeTests >> testMethodsEnumeration [
	scope := ClyInheritedTraitScope of: ClyClassWithTraits.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyTraitChild1 localMethods).
	self assert: (result includesAll: ClyTraitRoot1 localMethods).
	self deny: (result includesAll: ClyTraitRoot1 classSide localMethods).
	self deny: (result includesAll: ClyClassWithTraits localMethods).
	self deny: (result includesAll: ClyClassWithTraits classSide localMethods)
]

{ #category : #tests }
ClyInheritedTraitScopeTests >> testMethodsEnumerationWhenBothMetaLevels [

	scope := ClyInheritedTraitScope of: ClyClassWithTraits localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | result add: each].
	
	self assert: (result includesAll: ClyTraitChild1 localMethods).
	self assert: (result includesAll: ClyTraitChild1 classSide localMethods).
	self assert: (result includesAll: ClyTraitRoot1 localMethods).
	self assert: (result includesAll: ClyTraitRoot1 classSide localMethods).
	self deny: (result includesAll: ClyClassWithTraits localMethods).
	self deny: (result includesAll: ClyClassWithTraits classSide localMethods).

]
