Class {
	#name : #ClyPackageExtensionScopeTest,
	#superclass : #ClyTypedScopeTest,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyPackageExtensionScopeTest >> extraBasisSample [
	^ClySubclassMOfClass1FromP2Mock package 
]

{ #category : #running }
ClyPackageExtensionScopeTest >> scopeClass [

	^ClyPackageExtensionScope
]

{ #category : #running }
ClyPackageExtensionScopeTest >> singleBasisSample [
	^ClyClass1FromP1Mock package 
]

{ #category : #tests }
ClyPackageExtensionScopeTest >> testClassesEnumeration [
	scope := ClyPackageExtensionScope of: ClyClass1FromP1Mock package.
	
	scope classesDo: [ :each | result add: each ].
	
	self deny: (result includesAll: ClyClass1FromP1Mock package definedClasses).
	self assert: (result includesAll: ClyClass1FromP1Mock package extendedClasses)
]

{ #category : #tests }
ClyPackageExtensionScopeTest >> testMethodsEnumeration [
	scope := ClyPackageExtensionScope of: ClyClass1FromP1Mock package.
	
	scope methodsDo: [ :each | result add: each ].
	
	self deny: (result includesAll: ClyClass1FromP1Mock localMethods).
	self deny: (result includesAll: ClyClass2FromP1Mock classSide localMethods).
	self assert: (result includesAll: ClySubclassLOfClass0FromPExtendedByP1Mock localMethods)
]

{ #category : #tests }
ClyPackageExtensionScopeTest >> testPackageEnumeration [
	scope := ClyPackageExtensionScope ofAll: {Array package. Object package}.
	
	scope packagesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {Array package. Object package}
]
