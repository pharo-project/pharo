Class {
	#name : #ClyOverriddenMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverriddenMethodsQueryTest >> createQuery [
	^ ClyOverriddenMethodsQuery from: ClyClassScope of: ClyAbstractClassExample in: environment
]

{ #category : #running }
ClyOverriddenMethodsQueryTest >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope 
		of: (ClyAbstractClassExample >> #abstractMethod1)	in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyAbstractClassExample >> #overriddenMethod) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyAbstractClassExample >> #abstractMethod2) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyAbstractClassExample >> #abstractMethod1. 
		ClyAbstractClassExample >> #abstractMethod2.
		ClyAbstractClassExample >> #overriddenMethod}.
	
	self assert: foundSelectors equals: #(abstractMethod1 overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testIsAffectedByChangeOfOwnMethodWhichIsNotOverridden [
	query := ClyOverriddenMethodsQuery 
		from: ClyClassScope of: ClyClassWhichInheritsAbstractClass in: environment.
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass >>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testIsAffectedByChangeOfOwnMethodWhichIsOverridden [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassExample>>#overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testIsAffectedByChangeOfSubclassMethodWhichOverridesSuperclass [
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass>>#overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testNotAffectedByChangeOfSubclassMethodWhichNotOverridesSuperclass [
	
	self deny: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testNotSelectsNotOverriddenMethod [

	self deny: (query selectsMethod: ClyClassWhichInheritsAbstractClass>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testSelectsOverriddenMethod [

	self assert: (query selectsMethod: ClyAbstractClassExample>>#overriddenMethod)
]
