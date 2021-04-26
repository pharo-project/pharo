Class {
	#name : #ClyScopeTest,
	#superclass : #ClyAbsractNavigationEnvironmentTest,
	#instVars : [
		'scope',
		'result'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyScopeTest class >> isAbstract [
	^self == ClyScopeTest
]

{ #category : #running }
ClyScopeTest >> createSampleScope [
	self subclassResponsibility 
]

{ #category : #running }
ClyScopeTest >> setUp [
	super setUp.
	
	"For tests we want predictable order or collected items to simplify assertions"
	result := SortedCollection sortBlock: #printString ascending
]

{ #category : #tests }
ClyScopeTest >> testBindToEnvironment [

	scope := self createSampleScope.

	scope bindTo: environment.
	
	self assert: scope environment equals: environment.
	self assert: scope isBoundToEnvironment
]

{ #category : #tests }
ClyScopeTest >> testComparisonWithCopy [

	| copy |
	scope := self createSampleScope.
	copy := scope copy.	
	self assert: scope equals: copy.
	self assert: scope hash equals: copy hash
]

{ #category : #tests }
ClyScopeTest >> testComparisonWithDifferentScope [
	| scope2 |
	scope := self createSampleScope.
	scope2 := ClyExample2Scope of: #basisObject in: environment.
	self deny: scope equals: scope2
]

{ #category : #tests }
ClyScopeTest >> testComparisonWithItself [

	scope := self createSampleScope.
	
	self assert: scope equals: scope
]

{ #category : #tests }
ClyScopeTest >> testComparisonWithSimilarScope [

	| scope2 |
	scope := self createSampleScope.
	scope2 := self createSampleScope.
	
	self assert: scope equals: scope2.
	self assert: scope hash equals: scope2 hash
]

{ #category : #tests }
ClyScopeTest >> testConvertingToAnotherScopeClass [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asScope: ClyExampleScope.
	
	self assert: convertedScope class equals: ClyExampleScope.
	self assert: convertedScope basisObjects asIdentitySet equals: scope basisObjects asIdentitySet.
	self assert: convertedScope environment equals: environment
]

{ #category : #tests }
ClyScopeTest >> testConvertingToAnotherScopeClassShouldKeepName [
	| convertedScope |
	scope := self createSampleScope.
	scope name: 'test scope'.
	
	convertedScope := scope asScope: ClyExampleScope.
	
	self assert: convertedScope name equals: 'test scope'.
	
]
