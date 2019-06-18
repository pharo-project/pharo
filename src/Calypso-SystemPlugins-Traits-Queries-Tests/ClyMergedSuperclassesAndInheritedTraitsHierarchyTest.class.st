Class {
	#name : #ClyMergedSuperclassesAndInheritedTraitsHierarchyTest,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemPlugins-Traits-Queries-Tests'
}

{ #category : #running }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> createQueryResult [
	^(ClySubclassHierarchy inverse, ClyTraitUserHierarchy inverse) 
		asQueryResult sortedBy: ClyTraitFirstSortFunction ascending
]

{ #category : #tests }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassAndItsDeepTwoTraits [
	
	queryResult fillWith: {ClyClassWithTraits. ClyTraitChild2. ClyTraitRoot2 }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild2. 1. ClyTraitRoot2. 2. 
			ClyTraitRoot2. 1}
]

{ #category : #tests }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassAndItsTraits [
	
	queryResult fillWith: {
		ClyClassWithTraits. ClyTraitChild1. ClyTraitChild2. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild1. 1. ClyTraitRoot1. 2. 
			ClyTraitChild2Child. 1. ClyTraitChild2. 2. ClyTraitRoot1. 3}.
]

{ #category : #tests }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassAndItsTraitsWhenSomeMiddleTraitIsMissing [
	
	queryResult fillWith: {
		ClyClassWithTraits. ClyTraitChild1. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild1. 1. ClyTraitRoot1. 2. 
			ClyTraitChild2Child. 1. ClyTraitRoot1. 2}.
]

{ #category : #tests }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassWithoutTraits [
	
	queryResult fillWith: {ClyClassWithoutTraits}.
		
	self assertItems: {ClyClassWithoutTraits. 0}
]

{ #category : #tests }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> testFillingWithOnlyClassWhichHasTraits [
	
	queryResult fillWith: {ClyClassWithTraits}.
		
	self assertItems: {ClyClassWithTraits. 0}
]

{ #category : #tests }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> testFillingWithTraitsOfClassWhichIsMissingInGivenList [
	
	queryResult fillWith: {
		ClyTraitChild1. ClyTraitChild2. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyTraitChild1. 0. ClyTraitRoot1. 1. 
		ClyTraitChild2Child. 0. ClyTraitChild2. 1. ClyTraitRoot1. 2}.
]

{ #category : #tests }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> testFillingWithTwoClassWhichInheriteSameTraits [
	
	queryResult fillWith: {ClyClassWithTraits. ClyTraitChild2. ClyTraitRoot2.
		ClySubclassWithTraitsOfClassWithTraits "inherits Root2 in deep chain"}.
		
	self assertItems: {
		ClySubclassWithTraitsOfClassWithTraits. 0.
			ClyTraitRoot2. 1.
			ClyClassWithTraits. 1.
				ClyTraitChild2. 2. ClyTraitRoot2. 3. 
				ClyTraitRoot2. 2}
]

{ #category : #tests }
ClyMergedSuperclassesAndInheritedTraitsHierarchyTest >> testFillingWithTwoClassWhichInheriteSameTraitsAndTheirSuperclasses [
	
	queryResult fillWith: {ClyClassWithTraits. ClyTraitChild2. ClyTraitRoot2.
		ClySubclassWithTraitsOfClassWithTraits. Object. ProtoObject}.
		
	self assertItems: {
		ClySubclassWithTraitsOfClassWithTraits. 0.
			ClyTraitRoot2. 1.
			ClyClassWithTraits. 1.
				ClyTraitChild2. 2. ClyTraitRoot2. 3. 
				ClyTraitRoot2. 2.
				Object. 2. ProtoObject. 3}
]
