Class {
	#name : #ClyGroupedExtendingPackagesTests,
	#superclass : #ClyQueryResultTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyGroupedExtendingPackagesTests >> createQueryResult [
	^ClyGroupedExtendingPackages new
]

{ #category : #tests }
ClyGroupedExtendingPackagesTests >> testFillingWithEmptyList [

	queryResult fillWith: #().
	
	self assert: queryResult size equals: 0
]

{ #category : #tests }
ClyGroupedExtendingPackagesTests >> testFillingWithOnePackage [

	| group |
	queryResult fillWith: {ClyClass1FromP1 package}.
	
	group := queryResult items first.
	self assert: group class equals: ClyExternalPackageMethodGroup.
	self assert: group package equals: ClyClass1FromP1 package.
	self 
		assert: group methodQuery 
		equals: (ClyPackageExtensionMethods of: ClyClass1FromP1 package from: query scope)
]

{ #category : #tests }
ClyGroupedExtendingPackagesTests >> testFillingWithThreePackages [

	| groups |
	queryResult fillWith: { ClySubclassMOfClass1FromP2 package. 
		ClySubclassLOfSubclassMFromP3 package.
		ClyClass1FromP1 package}.
	groups := queryResult items collect: #package as: Array.
	self assert: groups equals: { ClyClass1FromP1 package.
		ClySubclassMOfClass1FromP2 package.
		ClySubclassLOfSubclassMFromP3 package}
]
