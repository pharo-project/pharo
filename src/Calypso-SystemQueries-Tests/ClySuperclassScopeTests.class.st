Class {
	#name : #ClySuperclassScopeTests,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClySuperclassScopeTests >> scopeClass [
	
	^ClySuperclassScope
]

{ #category : #tests }
ClySuperclassScopeTests >> testClassEnumeration [
	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1.
	
	scope classesDo: [ :each | result add: each ].
	
	expected := ClyClass1FromP1 allSuperclasses.
	self assert: result asArray equals: expected asArray
]

{ #category : #tests }
ClySuperclassScopeTests >> testClassEnumerationOverInstanceSideBasisWhenBothMetaLevelsAreLocalScope [

	scope := ClySuperclassScope of: ClyClass1FromP1 localScope: ClyBothMetaLevelClassScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {Object. Object class. ProtoObject. ProtoObject class } asSet
]

{ #category : #tests }
ClySuperclassScopeTests >> testClassEnumerationOverMetaclassBasisWhenInstanceSideIsLocalScope [

	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyInstanceSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {Object. ProtoObject } asSet
]

{ #category : #tests }
ClySuperclassScopeTests >> testClassEnumerationOverMetaclassWhenBothMetaLevelsAreLocalScope [

	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyBothMetaLevelClassScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {Object. Object class. ProtoObject. ProtoObject class } asSet
]

{ #category : #tests }
ClySuperclassScopeTests >> testClassEnumerationOverMetaclassWhenClassSideIsLocalScope [

	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyClassSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {Object class. ProtoObject class } asSet
]

{ #category : #tests }
ClySuperclassScopeTests >> testClassEnumerationOverMetaclassWhenConcreteClassScopeIsLocal [

	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyClassScope.

	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: ClyClass1FromP1 class allSuperclasses asSet
]

{ #category : #tests }
ClySuperclassScopeTests >> testConvertingToInterestingClassScope [
	| convertedScope |
	scope := self createSampleScope.
	scope localScopeClass: ClyInstanceSideScope.
	
	convertedScope := scope asInterestingClassScope.
	
	self assert: convertedScope class equals: ClyInterestingSuperclassScope.
	self assert: convertedScope basisObjects equals: scope basisObjects.
	self assert: convertedScope localScopeClass equals: scope localScopeClass.
	self assert: convertedScope environment equals: scope environment
]

{ #category : #tests }
ClySuperclassScopeTests >> testMethodsEnumeration [
	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1.
	
	scope methodsDo: [ :each | result add: each ].
	
	expected := {Object.
	ProtoObject} flatCollect: #localMethods as: IdentitySet.
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTests >> testMethodsEnumerationOverInstanceSideWhenBothMetaLevelsAreLocalScope [

	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1 localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | result add: each].

	expected := { Object. Object class. ProtoObject. ProtoObject class  }
		flatCollect: #localMethods as: IdentitySet..
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTests >> testMethodsEnumerationOverMetaclassWhenBothMetaLevelsAreLocalScope [

	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | result add: each].

	expected := { Object. Object class. ProtoObject. ProtoObject class  }
		flatCollect: #localMethods as: IdentitySet..
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTests >> testMethodsEnumerationOverMetaclassWhenClassSideIsLocalScope [

	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyClassSideScope.
	
	scope methodsDo: [ :each | result add: each].

	expected := { Object class. ProtoObject class  } flatCollect: #localMethods as: IdentitySet..
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTests >> testMethodsEnumerationOverMetaclassWhenConcreteClassScopeIsScope [

	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyClassScope.
	
	scope methodsDo: [ :each | result add: each].

	"Now we should see more classes than Object and ProtoObject 
	because ProtoObject class has more superclasses including Object and ProtoObject itself"
	expected := ClyClass1FromP1 class allSuperclasses flatCollect: #localMethods as: IdentitySet..
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTests >> testMethodsEnumerationWithBothMetaLevelScopeShouldNotDublicateMethods [
	| expected |
	scope := ClySuperclassScope of: ClySubclassN1OfClass1FromP1 localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | 
		each methodClass instanceSide = ClyClass1FromP1 ifTrue: [result add: each] ].
	
	expected := (ClyClass1FromP1 localMethods , ClyClass1FromP1 class localMethods) asIdentitySet.
	self assert: result asIdentitySet equals: expected
]
