Class {
	#name : #ClyOverridingMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverridingMethodsTests >> createQuery [
	^ClyOverridingMethods from: ClyClassScope of: ClyClassWhichInheritsAbstractClass in: environment
]

{ #category : #running }
ClyOverridingMethodsTests >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]

{ #category : #tests }
ClyOverridingMethodsTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClyClassWhichInheritsAbstractClass >> #abstractMethod1) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWhichInheritsAbstractClass >> #overriddenMethod) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWhichInheritsAbstractClass >> #notOverriddenMethod) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyOverridingMethodsTests >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWhichInheritsAbstractClass >> #abstractMethod1. 
		ClyClassWhichInheritsAbstractClass >> #notOverriddenMethod.
		ClyClassWhichInheritsAbstractClass >> #overriddenMethod}.
	
	self assert: foundSelectors equals: #(abstractMethod1 #overriddenMethod)
]

{ #category : #tests }
ClyOverridingMethodsTests >> testIsAffectedByChangeOfOwnMethodWhichIsNotOverridden [
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass >> #notOverriddenMethod)

]

{ #category : #tests }
ClyOverridingMethodsTests >> testIsAffectedByChangeOfSuperclassMethodWhichIsAlreadyImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassExample >> #overriddenMethod)

]

{ #category : #tests }
ClyOverridingMethodsTests >> testIsAffectedByChangeOfSuperclassMethodWhichOverridesAnotherSuperclassMethod [
	"this case is important to be affected when override method was removed"
	
	query := ClyOverridingMethods 
		from: ClyClassScope of: ClyClassWhichInheritsAbstractSubclass in: environment.
		
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassExample >> #abstractMethod1)

]

{ #category : #tests }
ClyOverridingMethodsTests >> testNotAffectedByChangeOfOverriddenMethodFromAnotherHierarchyWhenSameMethodExistsHereToo [
	
	self deny: (query isAffectedByChangedMethod: ClyAbstractSubclassExample >> #abstractMethod1)

]

{ #category : #tests }
ClyOverridingMethodsTests >> testNotSelectsNotOverrideMethod [

	self deny: (query selectsMethod: ClyClassWhichInheritsAbstractClass>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverridingMethodsTests >> testSelectsOverrideMethod [

	self assert: (query selectsMethod: ClyClassWhichInheritsAbstractClass>>#overriddenMethod)
]
