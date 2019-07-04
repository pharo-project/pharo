Class {
	#name : #ClySuperclassScopeTest,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClySuperclassScopeTest >> scopeClass [
	
	^ClySuperclassScope
]

{ #category : #tests }
ClySuperclassScopeTest >> testClassEnumeration [
	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1.
	
	scope classesDo: [ :each | result add: each ].
	
	expected := ClyClass1FromP1 allSuperclasses.
	self assert: result asArray equals: expected asArray
]

{ #category : #tests }
ClySuperclassScopeTest >> testClassEnumerationOverInstanceSideBasisWhenBothMetaLevelsAreLocalScope [

	scope := ClySuperclassScope of: ClyClass1FromP1 localScope: ClyBothMetaLevelClassScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {Object. Object class. ProtoObject. ProtoObject class } asSet
]

{ #category : #tests }
ClySuperclassScopeTest >> testClassEnumerationOverMetaclassBasisWhenInstanceSideIsLocalScope [

	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyInstanceSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {Object. ProtoObject } asSet
]

{ #category : #tests }
ClySuperclassScopeTest >> testClassEnumerationOverMetaclassWhenBothMetaLevelsAreLocalScope [

	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyBothMetaLevelClassScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {Object. Object class. ProtoObject. ProtoObject class } asSet
]

{ #category : #tests }
ClySuperclassScopeTest >> testClassEnumerationOverMetaclassWhenClassSideIsLocalScope [

	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyClassSideScope.
	
	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: {Object class. ProtoObject class } asSet
]

{ #category : #tests }
ClySuperclassScopeTest >> testClassEnumerationOverMetaclassWhenConcreteClassScopeIsLocal [

	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyClassScope.

	scope classesDo: [ :each | result add: each].
	
	self assert: result asSet equals: ClyClass1FromP1 class allSuperclasses asSet
]

{ #category : #tests }
ClySuperclassScopeTest >> testConvertingToInterestingClassScope [
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
ClySuperclassScopeTest >> testMethodsEnumeration [
	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1.
	
	scope methodsDo: [ :each | result add: each ].
	
	expected := {Object.
	ProtoObject} flatCollect: #localMethods as: IdentitySet.
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTest >> testMethodsEnumerationOverInstanceSideWhenBothMetaLevelsAreLocalScope [

	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1 localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | result add: each].

	expected := { Object. Object class. ProtoObject. ProtoObject class  }
		flatCollect: #localMethods as: IdentitySet..
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTest >> testMethodsEnumerationOverMetaclassWhenBothMetaLevelsAreLocalScope [

	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | result add: each].

	expected := { Object. Object class. ProtoObject. ProtoObject class  }
		flatCollect: #localMethods as: IdentitySet..
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTest >> testMethodsEnumerationOverMetaclassWhenClassSideIsLocalScope [

	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyClassSideScope.
	
	scope methodsDo: [ :each | result add: each].

	expected := { Object class. ProtoObject class  } flatCollect: #localMethods as: IdentitySet..
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTest >> testMethodsEnumerationOverMetaclassWhenConcreteClassScopeIsScope [

	| expected |
	scope := ClySuperclassScope of: ClyClass1FromP1 class localScope: ClyClassScope.
	
	scope methodsDo: [ :each | result add: each].

	"Now we should see more classes than Object and ProtoObject 
	because ProtoObject class has more superclasses including Object and ProtoObject itself"
	expected := ClyClass1FromP1 class allSuperclasses flatCollect: #methods as: IdentitySet.
	self assert: result asIdentitySet equals: expected
]

{ #category : #tests }
ClySuperclassScopeTest >> testMethodsEnumerationWithBothMetaLevelScopeShouldNotDublicateMethods [
	| expected |
	scope := ClySuperclassScope of: ClySubclassN1OfClass1FromP1 localScope: ClyBothMetaLevelClassScope.
	
	scope methodsDo: [ :each | 
		each methodClass instanceSide = ClyClass1FromP1 ifTrue: [result add: each] ].
	
	expected := (ClyClass1FromP1 localMethods , ClyClass1FromP1 class localMethods) asIdentitySet.
	self assert: result asIdentitySet equals: expected
]
