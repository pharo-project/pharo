Class {
	#name : #ClyHierarchicalSystemItemsTest,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #helpers }
ClyHierarchicalSystemItemsTest >> createQueryResult [
	^ClyHierarchicalSystemItems new
]

{ #category : #tests }
ClyHierarchicalSystemItemsTest >> testFillingWithClassesFromOneHierarchy [

	queryResult fillWith: { ClySubclassN1OfClass1FromP1. ClySubclassLOfSubclassMFromP3. ClyClass1FromP1. ClySubclassN2OfClass1FromP1 }.

	self assertItems: {
		ClyClass1FromP1. 0. ClySubclassLOfSubclassMFromP3. 1. 
		ClySubclassN1OfClass1FromP1. 1. ClySubclassN2OfClass1FromP1. 1 }
]

{ #category : #tests }
ClyHierarchicalSystemItemsTest >> testFillingWithMethodsAndClasses [

	queryResult fillWith: {ClyClass1FromP1. ClyClass1FromP1 >> #tag1Method1}.

	self 
		assertItems: #(tag1Method1 0 ClyClass1FromP1 0) 
		withValue: [ :each | each name ]
]

{ #category : #tests }
ClyHierarchicalSystemItemsTest >> testFillingWithMethodsOfSameClass [

	queryResult fillWith: (
		#(tag1Method1 instanceSideVar1WriterMethod instanceSideVar1ReaderMethod) 
			collect: [:each | ClyClass1FromP1 >> each]).

	self 
		assertItems: #(#instanceSideVar1ReaderMethod 0 #instanceSideVar1WriterMethod 0 #tag1Method1 0) 
		withValue: [ :each | each name ]
]

{ #category : #tests }
ClyHierarchicalSystemItemsTest >> testFillingWithSuperclassAndSubclassMethods [

	queryResult fillWith: ClyClass1FromP1 localMethods, ClySubclassN1OfClass1FromP1 localMethods.

	self 
		assertItems: #(#instanceSideVar1ReaderMethod 0 #instanceSideVar1WriterMethod 0
			#superclassTag1Method 0 #tag1Method1 0 
			#noTagMethod 1 tag1Method1 1 tag1Method2 1 tag2Method1 1) 		
		withValue: [ :each | each name ]
]
