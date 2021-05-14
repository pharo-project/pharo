Class {
	#name : #ClyOverridingMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverridingMethodsQueryTest >> createQuery [
	^ClyOverridingMethodsQuery from: ClyClassScope of: ClyClassWhichInheritsAbstractClassMock in: environment
]

{ #category : #running }
ClyOverridingMethodsQueryTest >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClyClassWhichInheritsAbstractClassMock >> #abstractMethod1) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWhichInheritsAbstractClassMock >> #overriddenMethod) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWhichInheritsAbstractClassMock >> #notOverriddenMethod) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWhichInheritsAbstractClassMock >> #abstractMethod1. 
		ClyClassWhichInheritsAbstractClassMock >> #notOverriddenMethod.
		ClyClassWhichInheritsAbstractClassMock >> #overriddenMethod}.
	
	self assert: foundSelectors equals: #(abstractMethod1 #overriddenMethod)
]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testIsAffectedByChangeOfOwnMethodWhichIsNotOverridden [
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClassMock >> #notOverriddenMethod)

]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testIsAffectedByChangeOfSuperclassMethodWhichIsAlreadyImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassMock >> #overriddenMethod)

]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testIsAffectedByChangeOfSuperclassMethodWhichOverridesAnotherSuperclassMethod [
	"this case is important to be affected when override method was removed"
	
	query := ClyOverridingMethodsQuery 
		from: ClyClassScope of: ClyClassWhichInheritsAbstractSubclassMock in: environment.
		
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassMock >> #abstractMethod1)

]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testNotAffectedByChangeOfOverriddenMethodFromAnotherHierarchyWhenSameMethodExistsHereToo [
	
	self deny: (query isAffectedByChangedMethod: ClyAbstractSubclassMock >> #abstractMethod1)

]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testNotSelectsNotOverrideMethod [

	self deny: (query selectsMethod: ClyClassWhichInheritsAbstractClassMock>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverridingMethodsQueryTest >> testSelectsOverrideMethod [

	self assert: (query selectsMethod: ClyClassWhichInheritsAbstractClassMock>>#overriddenMethod)
]
