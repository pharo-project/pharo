Class {
	#name : #ClySubclassScopeTests,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClySubclassScopeTests >> scopeClass [
	
	^ClySubclassScope
]

{ #category : #tests }
ClySubclassScopeTests >> testClassEnumeration [
	| expected |
	scope := ClySubclassScope of: ClyClass1FromP1.
	
	scope classesDo: [ :each | result add: each ].
	
	expected := ClyClass1FromP1 allSubclasses.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTests >> testClassEnumerationOverClassWhenBothMetaLevelsAreLocalScope [

	| expected |
	scope := ClySubclassScope of: Class localScope: ClyBothMetaLevelClassScope.

	scope classesDo: [ :each | result add: each].
	
	expected := Class subclasses select: #isInstanceSide.
	expected := expected flatCollect: [:each | {each instanceSide. each classSide}].
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTests >> testClassEnumerationOverClassWhenClassSideIsLocalScope [

	| expected |
	scope := ClySubclassScope of: Class localScope: ClyClassSideScope.

	scope classesDo: [ :each | result add: each].
	
	expected := Class subclasses select: #isInstanceSide thenCollect: #classSide.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTests >> testClassEnumerationOverClassWhenConcreteClassScopeIsLocal [

	| expected |
	scope := ClySubclassScope of: Class localScope: ClyClassScope.

	scope classesDo: [ :each | result add: each].
	
	expected := Class allSubclasses asSet.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTests >> testClassEnumerationOverClassWhenInstanceSideIsLocalScope [

	| expected |
	scope := ClySubclassScope of: Class localScope: ClyInstanceSideScope.

	scope classesDo: [ :each | result add: each].
	
	expected := Class subclasses select: #isInstanceSide.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTests >> testClassEnumerationOverMetaClass [
	| expected |
	scope := ClySubclassScope of: ClyClass1FromP1 class.
	
	scope classesDo: [ :each | result add: each ].
	
	expected := ClyClass1FromP1 allSubclasses collect: #classSide.
	self assert: result asSet equals: expected asSet
]

{ #category : #tests }
ClySubclassScopeTests >> testMethodsEnumeration [
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
ClySubclassScopeTests >> testMethodsEnumerationWhenBothMetaLevels [

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
ClySubclassScopeTests >> testMethodsEnumerationWhenInstanceSideMetaLevel [
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
