Class {
	#name : #ClyAbstractClassScopeTest,
	#superclass : #ClyTypedScopeTest,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #testing }
ClyAbstractClassScopeTest class >> isAbstract [
	^self == ClyAbstractClassScopeTest 
]

{ #category : #running }
ClyAbstractClassScopeTest >> extraBasisSample [ 	
	^ClyClass2FromP1Mock
]

{ #category : #running }
ClyAbstractClassScopeTest >> singleBasisSample [ 	
	^ClyClass1FromP1Mock
]

{ #category : #tests }
ClyAbstractClassScopeTest >> testConvertingToIncreasedScopeByExtraClassScope [
	| convertedScope extraClassScope |
	scope := self singleScopeSample.
	
	extraClassScope := ClySuperclassScope of: self class in: environment.
	convertedScope := scope increasedByClassesFrom: extraClassScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: scope basisObjects, self class allSuperclasses.
]

{ #category : #tests }
ClyAbstractClassScopeTest >> testConvertingToInterestingClassScope [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asInterestingClassScope.
	
	self assert: convertedScope equals: scope
]

{ #category : #tests }
ClyAbstractClassScopeTest >> testConvertingToReducedScopeByExtraClassScope [

	| convertedScope extraClassScope increasedScope |
	scope := self multipleScopeSample.
	extraClassScope := ClySuperclassScope of: self class in: environment.
	increasedScope := scope increasedByClassesFrom: extraClassScope.
	convertedScope := increasedScope reducedByClassesFrom: extraClassScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: (scope basisObjects collect: [:each | each instanceSide])
]

{ #category : #tests }
ClyAbstractClassScopeTest >> testConvertingToSomeHierarchyScope [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asScope: ClySubclassScope.
	
	self assert: convertedScope class equals: ClySubclassScope.
	self assert: convertedScope localScopeClass equals: scope localScopeClass.
	self assert: convertedScope basisObjects equals: scope basisObjects.
	self assert: convertedScope environment equals: environment
]
