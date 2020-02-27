Class {
	#name : #ClyClassHierarchyScopeTestCase,
	#superclass : #ClyClassScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #testing }
ClyClassHierarchyScopeTestCase class >> isAbstract [
	^self = ClyClassHierarchyScopeTestCase
]

{ #category : #tests }
ClyClassHierarchyScopeTestCase >> testComparisonWithSimilarScopeButWithDifferentMetaLevelScope [
	| scope2 |
	scope := self createSampleScope.
	scope2 := self createSampleScope.

	self deny: scope localScopeClass equals: ClyInstanceSideScope.
	scope2 localScopeClass: ClyInstanceSideScope.
	self deny: scope equals: scope2
]

{ #category : #tests }
ClyClassHierarchyScopeTestCase >> testConvertingToInheritedScope [
	| convertedScope |
	scope := self createSampleScope.
	scope localScopeClass: ClyInstanceSideScope.
	
	convertedScope := scope asInheritedScope.
	
	self assert: (convertedScope representsScope: ClySuperclassScope).
	self assert: convertedScope localScopeClass equals: ClyInstanceSideScope.
	self assert: convertedScope basisObjects equals: scope basisObjects.
	self assert: convertedScope environment equals: environment.
	self assert: convertedScope name equals: ClyClassScope inheritedScopeName
]

{ #category : #tests }
ClyClassHierarchyScopeTestCase >> testConvertingToInheritingScope [
	| convertedScope |
	scope := self createSampleScope.
	scope localScopeClass: ClyInstanceSideScope.
	
	convertedScope := scope asInheritingScope.
	
	self assert: (convertedScope representsScope: ClySubclassScope).
	self assert: convertedScope localScopeClass equals: ClyInstanceSideScope.
	self assert: convertedScope basisObjects equals: scope basisObjects.
	self assert: convertedScope environment equals: environment.
	self assert: convertedScope name equals: ClyClassScope inheritingScopeName
]

{ #category : #tests }
ClyClassHierarchyScopeTestCase >> testConvertingToNewMetaLevel [
	| convertedScope |
	scope := self createSampleScope.
	self deny: scope localScopeClass equals: ClyInstanceSideScope.

	convertedScope := scope withMetaLevel: ClyInstanceSideScope.

	self assert: convertedScope class equals: scope class.
	self assert: convertedScope localScopeClass equals: ClyInstanceSideScope.
	self assert: convertedScope basisObjects equals: scope basisObjects.
	self assert: convertedScope environment equals: environment
]

{ #category : #tests }
ClyClassHierarchyScopeTestCase >> testConvertingWithNewBasisShouldKeepMetaLevelScope [
	| convertedScope |
	scope := self singleScopeSample.
	scope localScopeClass: ClyInstanceSideScope.
	
	convertedScope := scope withNewBasisObjects: {self extraBasisSample}.
	
	self assert: convertedScope localScopeClass equals: ClyInstanceSideScope 
]

{ #category : #tests }
ClyClassHierarchyScopeTestCase >> testHasDefaultClassLocalScope [

	scope := self createSampleScope.
	
	self assert: scope localScopeClass equals: ClyClassScope 
	
]
