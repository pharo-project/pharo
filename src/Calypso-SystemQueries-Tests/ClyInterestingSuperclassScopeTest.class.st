Class {
	#name : #ClyInterestingSuperclassScopeTest,
	#superclass : #ClyClassHierarchyScopeTest,
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
		of: ClySubclassN1OfClass1FromP1Mock class localScope: ClyClassSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {ClyClass1FromP1Mock class} asSet
]

{ #category : #tests }
ClyInterestingSuperclassScopeTest >> testClassEnumerationOverMetaclassBasisWhenInstanceSideIsLocalScope [

	scope := ClyInterestingSuperclassScope 
		of: ClySubclassN1OfClass1FromP1Mock class localScope: ClyInstanceSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {ClyClass1FromP1Mock } asSet
]

{ #category : #tests }
ClyInterestingSuperclassScopeTest >> testClassEnumerationShouldStopAtObject [
	scope := ClyInterestingSuperclassScope of: ClySubclassN1OfClass1FromP1Mock.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1Mock}
]

{ #category : #tests }
ClyInterestingSuperclassScopeTest >> testClassEnumerationWhenObjectIsFirstSuperclass [
	scope := ClyInterestingSuperclassScope of: ClyClass1FromP1Mock.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result isEmpty
]
