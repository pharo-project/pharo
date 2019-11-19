Class {
	#name : #ClyScopeTestCase,
	#superclass : #ClyNavigationEnvironmentTestCase,
	#instVars : [
		'scope',
		'result'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyScopeTestCase class >> isAbstract [
	^self == ClyScopeTestCase
]

{ #category : #running }
ClyScopeTestCase >> createSampleScope [
	self subclassResponsibility 
]

{ #category : #running }
ClyScopeTestCase >> setUp [
	super setUp.
	
	"For tests we want predictable order or collected items to simplify assertions"
	result := SortedCollection sortBlock: #printString ascending
]

{ #category : #tests }
ClyScopeTestCase >> testBindToEnvironment [

	scope := self createSampleScope.

	scope bindTo: environment.
	
	self assert: scope environment equals: environment.
	self assert: scope isBoundToEnvironment
]

{ #category : #tests }
ClyScopeTestCase >> testComparisonWithCopy [

	| copy |
	scope := self createSampleScope.
	copy := scope copy.	
	self assert: scope equals: copy.
	self assert: scope hash equals: copy hash
]

{ #category : #tests }
ClyScopeTestCase >> testComparisonWithDifferentScope [
	| scope2 |
	scope := self createSampleScope.
	scope2 := ClyScopeExample2 of: #basisObject in: environment.
	self deny: scope equals: scope2
]

{ #category : #tests }
ClyScopeTestCase >> testComparisonWithItself [

	scope := self createSampleScope.
	
	self assert: scope equals: scope
]

{ #category : #tests }
ClyScopeTestCase >> testComparisonWithSimilarScope [

	| scope2 |
	scope := self createSampleScope.
	scope2 := self createSampleScope.
	
	self assert: scope equals: scope2.
	self assert: scope hash equals: scope2 hash
]

{ #category : #tests }
ClyScopeTestCase >> testConvertingToAnotherScopeClass [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asScope: ClyScopeExample.
	
	self assert: convertedScope class equals: ClyScopeExample.
	self assert: convertedScope basisObjects asIdentitySet equals: scope basisObjects asIdentitySet.
	self assert: convertedScope environment equals: environment
]

{ #category : #tests }
ClyScopeTestCase >> testConvertingToAnotherScopeClassShouldKeepName [
	| convertedScope |
	scope := self createSampleScope.
	scope name: 'test scope'.
	
	convertedScope := scope asScope: ClyScopeExample.
	
	self assert: convertedScope name equals: 'test scope'.
	
]
