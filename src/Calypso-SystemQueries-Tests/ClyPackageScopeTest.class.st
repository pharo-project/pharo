Class {
	#name : #ClyPackageScopeTest,
	#superclass : #ClyTypedScopeTest,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyPackageScopeTest >> extraBasisSample [
	^ClySubclassMOfClass1FromP2Mock package 
]

{ #category : #running }
ClyPackageScopeTest >> scopeClass [

	^ClyPackageScope
]

{ #category : #running }
ClyPackageScopeTest >> singleBasisSample [
	^ClyClass1FromP1Mock package 
]

{ #category : #tests }
ClyPackageScopeTest >> testClassGroupEnumeration [
	environment addPlugin: ClyDefaultSystemEnvironmentPlugin new.
	scope := ClyPackageScope of: ClyClass7WithTag1FromP5Mock package in: environment.
	
	scope classGroupsDo: [ :each | result add: each ].
	
	self
		assert: (result collect: #class as: Set)
		equals: {ClyNoTagClassGroup. ClyTaggedClassGroup. ClyClassGroup} asSet.
	self
		assert: 
			(result
				select: [ :each | each class = ClyTaggedClassGroup ]
				thenCollect: #tag) sorted asArray
		equals: ClyClass7WithTag1FromP5Mock package tagsForClasses sorted asArray
]

{ #category : #tests }
ClyPackageScopeTest >> testClassesEnumeration [
	scope := ClyPackageScope of: ClyClass1FromP1Mock package.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1Mock package definedClasses).
	self deny: (result includesAll: ClyClass1FromP1Mock package extendedClasses)
]

{ #category : #tests }
ClyPackageScopeTest >> testMethodsEnumeration [
	scope := ClyPackageScope of: ClyClass1FromP1Mock package.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1Mock localMethods).
	self assert: (result includesAll: ClyClass2FromP1Mock localMethods).
	self
		assert: (result includesAll: ClyClass1FromP1Mock classSide localMethods)
		description: 'Package scope should see class side methods too'.
	self
		assert: (result includesAll: ClySubclassLOfClass0FromPExtendedByP1Mock localMethods)
		description: 'Package scope should see all clas extensions'
]

{ #category : #tests }
ClyPackageScopeTest >> testPackageEnumeration [
	scope := ClyPackageScope ofAll: {Array package. Object package}.
	
	scope packagesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {Array package. Object package}
]
