Class {
	#name : #ClyMergedSubclassesAndInheritedTraitsHierarchyTest,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemPlugins-Traits-Queries-Tests'
}

{ #category : #running }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> createQueryResult [
	^(ClySubclassHierarchy new, ClyTraitUserHierarchy inverse) 
		asQueryResult sortedBy: ClyTraitFirstSortFunction ascending
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassAndItGrandSuperclass [
	
	queryResult fillWith: { ClyClass1FromP1. ClySubclassLOfSubclassMFromP3 }.
	
	self assertItems: {ClyClass1FromP1. 0. ClySubclassLOfSubclassMFromP3. 1}
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassAndItsDeepTwoTraits [
	
	queryResult fillWith: {ClyClassWithTraits. ClyTraitChild2. ClyTraitRoot2 }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild2. 1. ClyTraitRoot2. 2. 
			ClyTraitRoot2. 1}
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassAndItsTraits [
	
	queryResult fillWith: {
		ClyClassWithTraits. ClyTraitChild1. ClyTraitChild2. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild1. 1. ClyTraitRoot1. 2. 
			ClyTraitChild2Child. 1. ClyTraitChild2. 2. ClyTraitRoot1. 3}.
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassAndItsTraitsWhenSomeMiddleTraitIsMissing [
	
	queryResult fillWith: {
		ClyClassWithTraits. ClyTraitChild1. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild1. 1. ClyTraitRoot1. 2. 
			ClyTraitChild2Child. 1. ClyTraitRoot1. 2}.
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassWithoutTraits [
	
	queryResult fillWith: {ClyClassWithoutTraits}.
		
	self assertItems: {ClyClassWithoutTraits. 0}
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithClassesFromOneHierarchy [

	queryResult fillWith: { ClySubclassN1OfClass1FromP1. ClySubclassLOfSubclassMFromP3. ClyClass1FromP1. ClySubclassN2OfClass1FromP1 }.

	self assertItems: {
		ClyClass1FromP1. 0. ClySubclassLOfSubclassMFromP3. 1. 
		ClySubclassN1OfClass1FromP1. 1. ClySubclassN2OfClass1FromP1. 1 }
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithObjectAndTwoProtoObjects [

	queryResult fillWith: { Object. ProtoObject. ProtoObject }.
	
	self assertItems: {ProtoObject. 0. Object. 1}
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithOnlyClassWhichHasTraits [
	
	queryResult fillWith: {ClyClassWithTraits}.
		
	self assertItems: {ClyClassWithTraits. 0}
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithTraitsOfClassWhichIsMissingInGivenList [
	
	queryResult fillWith: {
		ClyTraitChild1. ClyTraitChild2. ClyTraitRoot1. ClyTraitChild2Child }.
		
	self assertItems: {
		ClyTraitChild1. 0. ClyTraitRoot1. 1. 
		ClyTraitChild2Child. 0. ClyTraitChild2. 1. ClyTraitRoot1. 2}.
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithTwoClassWhichInheriteSameTraits [
	
	queryResult fillWith: {ClyClassWithTraits. ClyTraitChild2. ClyTraitRoot2.
		ClySubclassWithTraitsOfClassWithTraits "inherits Root2 in deep chain"}.
		
	self assertItems: {
		ClyClassWithTraits. 0.
			ClyTraitChild2. 1. ClyTraitRoot2. 2. 
			ClyTraitRoot2. 1.
			ClySubclassWithTraitsOfClassWithTraits. 1. 
				ClyTraitRoot2. 2}
]

{ #category : #tests }
ClyMergedSubclassesAndInheritedTraitsHierarchyTest >> testFillingWithTwoSameClasses [

	queryResult fillWith: { ClyClass1FromP1. ClyClass1FromP1 }.

	self assertItems: { ClyClass1FromP1. 0 }
]
