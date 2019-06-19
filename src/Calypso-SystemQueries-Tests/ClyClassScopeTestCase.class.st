Class {
	#name : #ClyClassScopeTestCase,
	#superclass : #ClyTypedScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #testing }
ClyClassScopeTestCase class >> isAbstract [
	^self == ClyClassScopeTestCase 
]

{ #category : #running }
ClyClassScopeTestCase >> extraBasisSample [ 	
	^ClyClass2FromP1
]

{ #category : #running }
ClyClassScopeTestCase >> singleBasisSample [ 	
	^ClyClass1FromP1
]

{ #category : #tests }
ClyClassScopeTestCase >> testConvertingToIncreasedScopeByExtraClassScope [
	| convertedScope extraClassScope |
	scope := self singleScopeSample.
	
	extraClassScope := ClySuperclassScope of: self class in: environment.
	convertedScope := scope increasedByClassesFrom: extraClassScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: scope basisObjects, self class allSuperclasses.
]

{ #category : #tests }
ClyClassScopeTestCase >> testConvertingToInterestingClassScope [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asInterestingClassScope.
	
	self assert: convertedScope equals: scope
]

{ #category : #tests }
ClyClassScopeTestCase >> testConvertingToReducedScopeByExtraClassScope [
	| convertedScope extraClassScope increasedScope |
	scope := self multipleScopeSample.
	extraClassScope := ClySuperclassScope of: self class in: environment.
	
	increasedScope := scope increasedByClassesFrom: extraClassScope.
	convertedScope := increasedScope reducedByClassesFrom: extraClassScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: (scope basisObjects collect: #instanceSide)
]

{ #category : #tests }
ClyClassScopeTestCase >> testConvertingToSomeHierarchyScope [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asScope: ClySubclassScope.
	
	self assert: convertedScope class equals: ClySubclassScope.
	self assert: convertedScope localScopeClass equals: scope localScopeClass.
	self assert: convertedScope basisObjects equals: scope basisObjects.
	self assert: convertedScope environment equals: environment
]
