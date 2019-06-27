Class {
	#name : #ClyOverridingMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverridingMethodsQueryTest >> createQuery [
	^ClyOverridingMethodsQuery from: ClyClassScope of: ClyClassWhichInheritsAbstractClass in: environment
]

{ #category : #running }
ClyOverridingMethodsQueryTest >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testCheckIfEmpty [
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
ClyOverridingMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWhichInheritsAbstractClass >> #abstractMethod1. 
		ClyClassWhichInheritsAbstractClass >> #notOverriddenMethod.
		ClyClassWhichInheritsAbstractClass >> #overriddenMethod}.
	
	self assert: foundSelectors equals: #(abstractMethod1 #overriddenMethod)
]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testIsAffectedByChangeOfOwnMethodWhichIsNotOverridden [
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass >> #notOverriddenMethod)

]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testIsAffectedByChangeOfSuperclassMethodWhichIsAlreadyImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassExample >> #overriddenMethod)

]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testIsAffectedByChangeOfSuperclassMethodWhichOverridesAnotherSuperclassMethod [
	"this case is important to be affected when override method was removed"
	
	query := ClyOverridingMethodsQuery 
		from: ClyClassScope of: ClyClassWhichInheritsAbstractSubclass in: environment.
		
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassExample >> #abstractMethod1)

]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testNotAffectedByChangeOfOverriddenMethodFromAnotherHierarchyWhenSameMethodExistsHereToo [
	
	self deny: (query isAffectedByChangedMethod: ClyAbstractSubclassExample >> #abstractMethod1)

]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testNotSelectsNotOverrideMethod [

	self deny: (query selectsMethod: ClyClassWhichInheritsAbstractClass>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testSelectsOverrideMethod [

	self assert: (query selectsMethod: ClyClassWhichInheritsAbstractClass>>#overriddenMethod)
]
