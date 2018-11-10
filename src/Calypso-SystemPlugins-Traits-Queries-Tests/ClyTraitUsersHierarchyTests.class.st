Class {
	#name : #ClyTraitUsersHierarchyTests,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemPlugins-Traits-Queries-Tests'
}

{ #category : #running }
ClyTraitUsersHierarchyTests >> createQueryResult [
	^ClyTraitUserHierarchy new asQueryResult
]

{ #category : #tests }
ClyTraitUsersHierarchyTests >> testFillingWithClassInsteadOfTrait [
	
	queryResult fillWith: {ClyClassWithoutTraits}.
		
	self assertItems: {ClyClassWithoutTraits. 0}
]

{ #category : #tests }
ClyTraitUsersHierarchyTests >> testFillingWithOnlyTraitWithoutUsers [
	
	queryResult fillWith: {ClyTraitRoot1}.
		
	self assertItems: { ClyTraitRoot1. 0} 
]

{ #category : #tests }
ClyTraitUsersHierarchyTests >> testFillingWithTraitAndItsUsers [
	
	queryResult fillWith: {
		ClyTraitRoot1. ClyTraitChild1. ClyClassWithTraits. ClyTraitChild2. ClyTraitChild2Child}.
		
	self assertItems: { 
		ClyTraitRoot1. 0.
			ClyTraitChild1. 1. ClyClassWithTraits. 2. 
			ClyTraitChild2. 1. ClyTraitChild2Child. 2. ClyClassWithTraits. 3
		 }
]

{ #category : #tests }
ClyTraitUsersHierarchyTests >> testFillingWithTraitAndItsUsersWhenSomeUserInMiddleIsMissing [
	
	queryResult fillWith: {
		ClyTraitRoot1. ClyTraitChild1. ClyClassWithTraits. ClyTraitChild2}.
		
	self assertItems: { 
		ClyTraitRoot1. 0.
			ClyTraitChild1. 1. ClyClassWithTraits. 2. 
			ClyTraitChild2. 1. ClyClassWithTraits. 2
		 }
]

{ #category : #tests }
ClyTraitUsersHierarchyTests >> testFillingWithUsersOfTraitWhenRootTraitIsMissingInGivenList [
	
	queryResult fillWith: {
		ClyTraitChild1. ClyClassWithTraits. ClyTraitChild2}.
		
	self assertItems: { 
		ClyTraitChild1. 0. ClyClassWithTraits. 1. 
		ClyTraitChild2. 0. ClyClassWithTraits. 1 }
]
