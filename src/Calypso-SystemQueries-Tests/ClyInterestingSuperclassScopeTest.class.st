Class {
	#name : #ClyInterestingSuperclassScopeTest,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyInterestingSuperclassScopeTest >> scopeClass [
	^ClyInterestingSuperclassScope 
]

{ #category : #tests }
ClyInterestingSuperclassScopeTest >> testClassEnumerationOverClassShouldStopAtObject [
	scope := ClyInterestingSuperclassScope of: Class.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asSet equals: {ClassDescription. Behavior} asSet
]

{ #category : #tests }
ClyInterestingSuperclassScopeTest >> testClassEnumerationOverMetaclassBasisWhenClassSideIsLocalScope [

	scope := ClyInterestingSuperclassScope 
		of: ClySubclassN1OfClass1FromP1 class localScope: ClyClassSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {ClyClass1FromP1 class} asSet
]

{ #category : #tests }
ClyInterestingSuperclassScopeTest >> testClassEnumerationOverMetaclassBasisWhenInstanceSideIsLocalScope [

	scope := ClyInterestingSuperclassScope 
		of: ClySubclassN1OfClass1FromP1 class localScope: ClyInstanceSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {ClyClass1FromP1 } asSet
]

{ #category : #tests }
ClyInterestingSuperclassScopeTest >> testClassEnumerationShouldStopAtObject [
	scope := ClyInterestingSuperclassScope of: ClySubclassN1OfClass1FromP1.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1}
]

{ #category : #tests }
ClyInterestingSuperclassScopeTest >> testClassEnumerationWhenObjectIsFirstSuperclass [
	scope := ClyInterestingSuperclassScope of: ClyClass1FromP1.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result isEmpty
]
