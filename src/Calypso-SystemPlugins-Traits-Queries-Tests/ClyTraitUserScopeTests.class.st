Class {
	#name : #ClyTraitUserScopeTests,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemPlugins-Traits-Queries-Tests'
}

{ #category : #running }
ClyTraitUserScopeTests >> scopeClass [
	
	^ClyTraitUserScope
]

{ #category : #tests }
ClyTraitUserScopeTests >> testClassEnumeration [
	scope := ClyTraitUserScope of: ClyTraitRoot1.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asSet equals: {
		ClyTraitChild1.	ClyTraitChild2. 
		ClyClassWithTraits. ClyTraitChild2Child. ClyTraitChild3.
		ClySubclassWithTraitsOfClassWithTraits} asSet
]

{ #category : #tests }
ClyTraitUserScopeTests >> testMethodsEnumeration [
	scope := ClyTraitUserScope of: ClyTraitRoot1.

	scope methodsDo: [ :each | result add: each ].

	self assert: (result includesAll: ClyTraitChild1 localMethods).
	self assert: (result includesAll: ClyClassWithTraits localMethods).
	self deny: (result includesAll: ClyClassWithTraits classSide localMethods).
	self deny: (result includesAll: ClyTraitRoot1 localMethods).
	self deny: (result includesAll: ClyTraitRoot1 classSide localMethods)
]

{ #category : #tests }
ClyTraitUserScopeTests >> testMethodsEnumerationWhenBothMetaLevels [

	scope := ClyTraitUserScope of: ClyTraitRoot1 localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | result add: each].
	
	self assert: (result includesAll: ClyTraitChild1 localMethods).
	self assert: (result includesAll: ClyTraitChild1 classSide localMethods).
	self assert: (result includesAll: ClyClassWithTraits localMethods).
	self assert: (result includesAll: ClyClassWithTraits classSide localMethods).
	self deny: (result includesAll: ClyTraitRoot1 localMethods).
	self deny: (result includesAll: ClyTraitRoot1 classSide localMethods).

]
