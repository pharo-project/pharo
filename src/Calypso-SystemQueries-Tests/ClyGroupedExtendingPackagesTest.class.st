Class {
	#name : #ClyGroupedExtendingPackagesTest,
	#superclass : #ClyQueryResultTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyGroupedExtendingPackagesTest >> createQueryResult [
	^ClyGroupedExtendingPackagesResult new
]

{ #category : #tests }
ClyGroupedExtendingPackagesTest >> testFillingWithEmptyList [

	queryResult fillWith: #().
	
	self assert: queryResult size equals: 0
]

{ #category : #tests }
ClyGroupedExtendingPackagesTest >> testFillingWithOnePackage [

	| group |
	queryResult fillWith: {ClyClass1FromP1Mock package}.
	
	group := queryResult items first.
	self assert: group class equals: ClyExternalPackageMethodGroup.
	self assert: group package equals: ClyClass1FromP1Mock package.
	self 
		assert: group methodQuery 
		equals: (ClyPackageExtensionMethodsQuery of: ClyClass1FromP1Mock package from: query scope)
]

{ #category : #tests }
ClyGroupedExtendingPackagesTest >> testFillingWithThreePackages [

	| groups |
	queryResult fillWith: { ClySubclassMOfClass1FromP2Mock package. 
		ClySubclassLOfSubclassMFromP3Mock package.
		ClyClass1FromP1Mock package}.
	groups := queryResult items collect: #package as: Array.
	self assert: groups equals: { ClyClass1FromP1Mock package.
		ClySubclassMOfClass1FromP2Mock package.
		ClySubclassLOfSubclassMFromP3Mock package}
]
