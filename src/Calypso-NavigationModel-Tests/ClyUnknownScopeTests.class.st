Class {
	#name : #ClyUnknownScopeTests,
	#superclass : #ClyScopeTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyUnknownScopeTests >> createSampleScope [
	^ClyUnknownScope new
]

{ #category : #tests }
ClyUnknownScopeTests >> testAdoptQuery [
	| query adoptedQuery |
	scope := self createSampleScope.
	query := ClyQueryExampleReturningScopeBasis new.
	
	adoptedQuery := scope adoptQuery: query.
	
	self assert: adoptedQuery == query
]

{ #category : #tests }
ClyUnknownScopeTests >> testConvertingToAnotherScopeClass [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asScope: ClyScopeExample.
	
	self assert: convertedScope == scope
]

{ #category : #tests }
ClyUnknownScopeTests >> testConvertingToNewBasis [
	scope := self createSampleScope.
	
	self assert: (scope withNewBasisObjects: #(newBasis)) == scope.
	self assert: scope basisObjects isEmpty
]

{ #category : #tests }
ClyUnknownScopeTests >> testCreationUnifiedInstance [
	scope := self createSampleScope.
	
	self assert: scope asUnifiedInstance == scope
]

{ #category : #tests }
ClyUnknownScopeTests >> testGettingSingletonInstanceFromScratch [

	ClyUnknownScope reset.
	
	self assert: ClyUnknownScope instance == ClyUnknownScope instance
]

{ #category : #tests }
ClyUnknownScopeTests >> testHasEmptyBasisObjects [

	scope := self createSampleScope.

	self assert: scope basisObjects isEmpty
]

{ #category : #tests }
ClyUnknownScopeTests >> testIsBasedOnEmptyBasis [

	scope := self createSampleScope.

	self assert: scope isBasedOnEmptyBasis
]

{ #category : #tests }
ClyUnknownScopeTests >> testIsNotBasedOnAnyObject [

	scope := self createSampleScope.

	self deny: (scope isBasedOn: #anyObject)
]

{ #category : #tests }
ClyUnknownScopeTests >> testIsNotBasedOnMultipleBasis [

	scope := self createSampleScope.

	self deny: scope isBasedOnMultipleBasis
]

{ #category : #tests }
ClyUnknownScopeTests >> testIsNotBasedOnSingleBasis [

	scope := self createSampleScope.

	self deny: scope isBasedOnSingleBasis
]
