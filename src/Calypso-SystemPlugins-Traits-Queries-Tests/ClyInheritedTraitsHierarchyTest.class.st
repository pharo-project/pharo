Class {
	#name : #ClyInheritedTraitsHierarchyTest,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemPlugins-Traits-Queries-Tests'
}

{ #category : #running }
ClyInheritedTraitsHierarchyTest >> createQueryResult [
	^ClyTraitUserHierarchy inverse asQueryResult 
]

{ #category : #tests }
ClyInheritedTraitsHierarchyTest >> testFillingWithClassAndItsDeepTwoTraits [
	
	queryResult fillWith: {ClyClassWithTraits. ClyTraitChild2. ClyTraitRoot2 }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild2. 1. ClyTraitRoot2. 2. 
			ClyTraitRoot2. 1}
]

{ #category : #tests }
ClyInheritedTraitsHierarchyTest >> testFillingWithClassAndItsTraits [
	
	queryResult fillWith: {
		ClyClassWithTraits. ClyTraitChild1. ClyTraitChild2. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild1. 1. ClyTraitRoot1. 2. 
			ClyTraitChild2Child. 1. ClyTraitChild2. 2. ClyTraitRoot1. 3}.
]

{ #category : #tests }
ClyInheritedTraitsHierarchyTest >> testFillingWithClassAndItsTraitsWhenSomeMiddleTraitIsMissing [
	
	queryResult fillWith: {
		ClyClassWithTraits. ClyTraitChild1. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild1. 1. ClyTraitRoot1. 2. 
			ClyTraitChild2Child. 1. ClyTraitRoot1. 2}.
]

{ #category : #tests }
ClyInheritedTraitsHierarchyTest >> testFillingWithClassWithoutTraits [
	
	queryResult fillWith: {ClyClassWithoutTraits}.
		
	self assertItems: {ClyClassWithoutTraits. 0}
]

{ #category : #tests }
ClyInheritedTraitsHierarchyTest >> testFillingWithOnlyClassWhichHasTraits [
	
	queryResult fillWith: {ClyClassWithTraits}.
		
	self assertItems: {ClyClassWithTraits. 0}
]

{ #category : #tests }
ClyInheritedTraitsHierarchyTest >> testFillingWithTraitsOfClassWhichIsMissingInGivenList [
	
	queryResult fillWith: {
		ClyTraitChild1. ClyTraitChild2. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyTraitChild1. 0. ClyTraitRoot1. 1. 
		ClyTraitChild2Child. 0. ClyTraitChild2. 1. ClyTraitRoot1. 2}.
]

{ #category : #tests }
ClyInheritedTraitsHierarchyTest >> testFillingWithTwoClassWhichInheriteSameTraits [
	
	queryResult fillWith: {ClyClassWithTraits. ClyTraitChild2. ClyTraitRoot2.
		ClySubclassWithTraitsOfClassWithTraits "inherits Root2 in deep chain"}.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild2. 1. ClyTraitRoot2. 2. 
			ClyTraitRoot2. 1.
		ClySubclassWithTraitsOfClassWithTraits. 0. ClyTraitRoot2. 1}
]
