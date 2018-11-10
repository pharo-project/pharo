Class {
	#name : #ClyInterestingSuperclassScopeTests,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyInterestingSuperclassScopeTests >> scopeClass [
	^ClyInterestingSuperclassScope 
]

{ #category : #tests }
ClyInterestingSuperclassScopeTests >> testClassEnumerationOverClassShouldStopAtObject [
	scope := ClyInterestingSuperclassScope of: Class.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asSet equals: {ClassDescription. Behavior} asSet
]

{ #category : #tests }
ClyInterestingSuperclassScopeTests >> testClassEnumerationOverMetaclassBasisWhenClassSideIsLocalScope [

	scope := ClyInterestingSuperclassScope 
		of: ClySubclassN1OfClass1FromP1 class localScope: ClyClassSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {ClyClass1FromP1 class} asSet
]

{ #category : #tests }
ClyInterestingSuperclassScopeTests >> testClassEnumerationOverMetaclassBasisWhenInstanceSideIsLocalScope [

	scope := ClyInterestingSuperclassScope 
		of: ClySubclassN1OfClass1FromP1 class localScope: ClyInstanceSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {ClyClass1FromP1 } asSet
]

{ #category : #tests }
ClyInterestingSuperclassScopeTests >> testClassEnumerationShouldStopAtObject [
	scope := ClyInterestingSuperclassScope of: ClySubclassN1OfClass1FromP1.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1}
]

{ #category : #tests }
ClyInterestingSuperclassScopeTests >> testClassEnumerationWhenObjectIsFirstSuperclass [
	scope := ClyInterestingSuperclassScope of: ClyClass1FromP1.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result isEmpty
]
