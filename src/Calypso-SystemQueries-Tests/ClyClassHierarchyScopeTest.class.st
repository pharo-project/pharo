Class {
	#name : #ClyClassHierarchyScopeTest,
	#superclass : #ClyAbstractClassScopeTest,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #testing }
ClyClassHierarchyScopeTest class >> isAbstract [
	^self = ClyClassHierarchyScopeTest
]

{ #category : #tests }
ClyClassHierarchyScopeTest >> testComparisonWithSimilarScopeButWithDifferentMetaLevelScope [
	| scope2 |
	scope := self createSampleScope.
	scope2 := self createSampleScope.

	self deny: scope localScopeClass equals: ClyInstanceSideScope.
	scope2 localScopeClass: ClyInstanceSideScope.
	self deny: scope equals: scope2
]

{ #category : #tests }
ClyClassHierarchyScopeTest >> testConvertingToInheritedScope [
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
ClyClassHierarchyScopeTest >> testConvertingToInheritingScope [
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
ClyClassHierarchyScopeTest >> testConvertingToNewMetaLevel [
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
ClyClassHierarchyScopeTest >> testConvertingWithNewBasisShouldKeepMetaLevelScope [
	| convertedScope |
	scope := self singleScopeSample.
	scope localScopeClass: ClyInstanceSideScope.
	
	convertedScope := scope withNewBasisObjects: {self extraBasisSample}.
	
	self assert: convertedScope localScopeClass equals: ClyInstanceSideScope 
]

{ #category : #tests }
ClyClassHierarchyScopeTest >> testHasDefaultClassLocalScope [

	scope := self createSampleScope.
	
	self assert: scope localScopeClass equals: ClyClassScope 
	
]
