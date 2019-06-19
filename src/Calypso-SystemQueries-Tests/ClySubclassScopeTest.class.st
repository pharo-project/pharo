Class {
	#name : #ClySubclassScopeTest,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClySubclassScopeTest >> scopeClass [
	
	^ClySubclassScope
]

{ #category : #tests }
ClySubclassScopeTest >> testClassEnumeration [
	| expected |
	scope := ClySubclassScope of: ClyClass1FromP1.
	
	scope classesDo: [ :each | result add: each ].
	
	expected := ClyClass1FromP1 allSubclasses.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTest >> testClassEnumerationOverClassWhenBothMetaLevelsAreLocalScope [

	| expected |
	scope := ClySubclassScope of: Class localScope: ClyBothMetaLevelClassScope.

	scope classesDo: [ :each | result add: each].
	
	expected := Class subclasses select: #isInstanceSide.
	expected := expected flatCollect: [:each | {each instanceSide. each classSide}].
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTest >> testClassEnumerationOverClassWhenClassSideIsLocalScope [

	| expected |
	scope := ClySubclassScope of: Class localScope: ClyClassSideScope.

	scope classesDo: [ :each | result add: each].
	
	expected := Class subclasses select: #isInstanceSide thenCollect: #classSide.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTest >> testClassEnumerationOverClassWhenConcreteClassScopeIsLocal [

	| expected |
	scope := ClySubclassScope of: Class localScope: ClyClassScope.

	scope classesDo: [ :each | result add: each].
	
	expected := Class allSubclasses asSet.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTest >> testClassEnumerationOverClassWhenInstanceSideIsLocalScope [

	| expected |
	scope := ClySubclassScope of: Class localScope: ClyInstanceSideScope.

	scope classesDo: [ :each | result add: each].
	
	expected := Class subclasses select: #isInstanceSide.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTest >> testClassEnumerationOverMetaClass [
	| expected |
	scope := ClySubclassScope of: ClyClass1FromP1 class.
	
	scope classesDo: [ :each | result add: each ].
	
	expected := ClyClass1FromP1 allSubclasses collect: #classSide.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTest >> testMethodsEnumeration [
	scope := ClySubclassScope of: ClyClass1FromP1.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1 subclasses first localMethods).
	self deny: (result includesAll: ClyClass1FromP1 subclasses first classSide localMethods).
	self assert: (result includesAll: ClyClass1FromP1 subclasses second localMethods).
	self deny: (result includesAll: ClyClass1FromP1 subclasses second classSide localMethods).
	self deny: (result includesAll: ClyClass1FromP1 localMethods).
	self deny: (result includesAll: ClyClass1FromP1 classSide localMethods).
	self deny: (result includesAll: ClyClass1FromP1 superclass localMethods).
	self deny: (result includesAll: ClyClass1FromP1 superclass classSide localMethods)
]

{ #category : #tests }
ClySubclassScopeTest >> testMethodsEnumerationWhenBothMetaLevels [

	scope := ClySubclassScope of: ClyClass1FromP1 localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | result add: each].
	
	self assert: (result includesAll: ClyClass1FromP1 subclasses first localMethods).
	self assert: (result includesAll: ClyClass1FromP1 subclasses first classSide localMethods).
	self assert: (result includesAll: ClyClass1FromP1 subclasses second localMethods).
	self assert: (result includesAll: ClyClass1FromP1 subclasses second classSide localMethods).
	self deny: (result includesAll: ClyClass1FromP1 localMethods).
	self deny: (result includesAll: ClyClass1FromP1 classSide localMethods).
	self deny: (result includesAll: ClyClass1FromP1 superclass localMethods).
	self deny: (result includesAll: ClyClass1FromP1 superclass classSide localMethods)
]

{ #category : #tests }
ClySubclassScopeTest >> testMethodsEnumerationWhenInstanceSideMetaLevel [
	| subclassMethods |
	scope := ClySubclassScope of: ClyClass1FromP1.
	scope localScopeClass: ClyInstanceSideScope.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1 subclasses first localMethods).
	subclassMethods := ClyClass1FromP1 subclasses
		flatCollect: [ :each | each classSide localMethods ].
	self assert: subclassMethods notEmpty.
	self deny: (result includesAll: subclassMethods)
]
