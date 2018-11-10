Class {
	#name : #ClyClassSideScopeTests,
	#superclass : #ClyLocalClassScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyClassSideScopeTests >> extraBasisSample [ 	
	^super extraBasisSample classSide
]

{ #category : #running }
ClyClassSideScopeTests >> scopeClass [

	^ClyClassSideScope
]

{ #category : #running }
ClyClassSideScopeTests >> singleBasisSample [ 	
	^super singleBasisSample classSide
]

{ #category : #tests }
ClyClassSideScopeTests >> testClassEnumeration [
	scope := ClyClassSideScope ofAll: {ClyClass1FromP1 classSide. ClyClass2FromP1}.
	
	scope classesDo: [ :each | result add: each ].
	
	self
		assert: result asArray
		equals: {ClyClass1FromP1 classSide. ClyClass2FromP1 classSide}
]

{ #category : #tests }
ClyClassSideScopeTests >> testConvertingToIncreasedScopeByExtraClassScope [
	| convertedScope extraClassScope |
	scope := self singleScopeSample.
	
	extraClassScope := ClySuperclassScope of: self class localScope: self scopeClass.
	convertedScope := scope increasedByClassesFrom: extraClassScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: scope basisObjects, extraClassScope collectAllClasses.
]

{ #category : #tests }
ClyClassSideScopeTests >> testMethodsEnumerationWhenBasisIsClass [
	| expected |
	scope := ClyClassSideScope of: ClyClass1FromP1.
	
	scope methodsDo: [ :each | result add: each selector ].
	
	expected := ClyClass1FromP1 classSide localMethods collect: #selector.
	self assert: result sorted asArray equals: expected sorted asArray
]

{ #category : #tests }
ClyClassSideScopeTests >> testMethodsEnumerationWhenBasisIsMetaclass [
	| expected |
	scope := ClyClassSideScope of: ClyClass1FromP1 classSide.
	
	scope methodsDo: [ :each | result add: each selector ].
	
	expected := ClyClass1FromP1 classSide localMethods collect: #selector.
	self assert: result sorted asArray equals: expected sorted asArray
]
