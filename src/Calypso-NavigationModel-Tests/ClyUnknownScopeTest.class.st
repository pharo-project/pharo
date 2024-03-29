Class {
	#name : 'ClyUnknownScopeTest',
	#superclass : 'ClyScopeTest',
	#category : 'Calypso-NavigationModel-Tests',
	#package : 'Calypso-NavigationModel-Tests'
}

{ #category : 'running' }
ClyUnknownScopeTest >> createSampleScope [
	^ClyUnknownScope new
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testAdoptQuery [
	| query adoptedQuery |
	scope := self createSampleScope.
	query := ClyReturningScopeBasisExampleQuery new.

	adoptedQuery := scope adoptQuery: query.

	self assert: adoptedQuery identicalTo: query
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testConvertingToAnotherScopeClass [
	| convertedScope |
	scope := self createSampleScope.

	convertedScope := scope asScope: ClyExampleScope.

	self assert: convertedScope identicalTo: scope
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testConvertingToNewBasis [
	scope := self createSampleScope.

	self assert: (scope withNewBasisObjects: #(newBasis)) identicalTo: scope.
	self assert: scope basisObjects isEmpty
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testCreationUnifiedInstance [
	scope := self createSampleScope.

	self assert: scope asUnifiedInstance identicalTo: scope
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testGettingSingletonInstanceFromScratch [
	ClyUnknownScope reset.

	self assert: ClyUnknownScope instance identicalTo: ClyUnknownScope instance
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testHasEmptyBasisObjects [

	scope := self createSampleScope.

	self assert: scope basisObjects isEmpty
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testIsBasedOnEmptyBasis [

	scope := self createSampleScope.

	self assert: scope isBasedOnEmptyBasis
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testIsNotBasedOnAnyObject [

	scope := self createSampleScope.

	self deny: (scope isBasedOn: #anyObject)
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testIsNotBasedOnMultipleBasis [

	scope := self createSampleScope.

	self deny: scope isBasedOnMultipleBasis
]

{ #category : 'tests' }
ClyUnknownScopeTest >> testIsNotBasedOnSingleBasis [

	scope := self createSampleScope.

	self deny: scope isBasedOnSingleBasis
]
