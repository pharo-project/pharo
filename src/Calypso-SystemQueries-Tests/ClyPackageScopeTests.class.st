Class {
	#name : #ClyPackageScopeTests,
	#superclass : #ClyTypedScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyPackageScopeTests >> extraBasisSample [
	^ClySubclassMOfClass1FromP2 package 
]

{ #category : #running }
ClyPackageScopeTests >> scopeClass [

	^ClyPackageScope
]

{ #category : #running }
ClyPackageScopeTests >> singleBasisSample [
	^ClyClass1FromP1 package 
]

{ #category : #tests }
ClyPackageScopeTests >> testClassGroupEnumeration [
	environment addPlugin: ClyDefaultSystemEnvironmentPlugin new.
	scope := ClyPackageScope of: ClyClass7WithTag1FromP5 package in: environment.
	
	scope classGroupsDo: [ :each | result add: each ].
	
	self
		assert: (result collect: #class as: Set)
		equals: {ClyNoTagClassGroup. ClyTaggedClassGroup. ClyClassGroup} asSet.
	self
		assert: 
			(result
				select: [ :each | each class = ClyTaggedClassGroup ]
				thenCollect: #tag) sorted asArray
		equals: ClyClass7WithTag1FromP5 package tagsForClasses sorted asArray
]

{ #category : #tests }
ClyPackageScopeTests >> testClassesEnumeration [
	scope := ClyPackageScope of: ClyClass1FromP1 package.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1 package definedClasses).
	self deny: (result includesAll: ClyClass1FromP1 package extendedClasses)
]

{ #category : #tests }
ClyPackageScopeTests >> testMethodsEnumeration [
	scope := ClyPackageScope of: ClyClass1FromP1 package.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1 localMethods).
	self assert: (result includesAll: ClyClass2FromP1 localMethods).
	self
		assert: (result includesAll: ClyClass1FromP1 classSide localMethods)
		description: 'Package scope should see class side methods too'.
	self
		assert: (result includesAll: ClySubclassLOfClass0FromPExtendedByP1 localMethods)
		description: 'Package scope should see all clas extensions'
]

{ #category : #tests }
ClyPackageScopeTests >> testPackageEnumeration [
	scope := ClyPackageScope ofAll: {Array package. Object package}.
	
	scope packagesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {Array package. Object package}
]
