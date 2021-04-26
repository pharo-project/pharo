Class {
	#name : #ClyHierarchicalSystemItemsTest,
	#superclass : #ClyBrowserQueryResultTest,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #helpers }
ClyHierarchicalSystemItemsTest >> createQueryResult [
	^ClyHierarchicalSystemItemsResult new
]

{ #category : #tests }
ClyHierarchicalSystemItemsTest >> testFillingWithClassesFromOneHierarchy [

	queryResult fillWith: { ClySubclassN1OfClass1FromP1Mock. ClySubclassLOfSubclassMFromP3Mock. ClyClass1FromP1Mock. ClySubclassN2OfClass1FromP1Mock }.

	self assertItems: {
		ClyClass1FromP1Mock. 0. ClySubclassLOfSubclassMFromP3Mock. 1. 
		ClySubclassN1OfClass1FromP1Mock. 1. ClySubclassN2OfClass1FromP1Mock. 1 }
]

{ #category : #tests }
ClyHierarchicalSystemItemsTest >> testFillingWithMethodsAndClasses [

	queryResult fillWith: {ClyClass1FromP1Mock. ClyClass1FromP1Mock >> #tag1Method1}.

	self 
		assertItems: #(tag1Method1 0 #ClyClass1FromP1Mock 0) 
		withValue: [ :each | each name ]
]

{ #category : #tests }
ClyHierarchicalSystemItemsTest >> testFillingWithMethodsOfSameClass [

	queryResult fillWith: (
		#(tag1Method1 instanceSideVar1WriterMethod instanceSideVar1ReaderMethod) 
			collect: [:each | ClyClass1FromP1Mock >> each]).

	self 
		assertItems: #(#instanceSideVar1ReaderMethod 0 #instanceSideVar1WriterMethod 0 #tag1Method1 0) 
		withValue: [ :each | each name ]
]

{ #category : #tests }
ClyHierarchicalSystemItemsTest >> testFillingWithSuperclassAndSubclassMethods [

	queryResult fillWith: ClyClass1FromP1Mock localMethods, ClySubclassN1OfClass1FromP1Mock localMethods.

	self 
		assertItems: #(#instanceSideVar1ReaderMethod 0 #instanceSideVar1WriterMethod 0 #instanceSideVar2 0 
			#superclassTag1Method 0 #tag1Method1 0 
			#noTagMethod 1 #subclassN1Var 1 #subclassSameVar 1 #tag1Method1 1 #tag1Method2 1 #tag2Method1 1 ) 		
		withValue: [ :each | each name ]
]
