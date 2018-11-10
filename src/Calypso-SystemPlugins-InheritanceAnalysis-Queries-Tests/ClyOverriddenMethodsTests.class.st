Class {
	#name : #ClyOverriddenMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverriddenMethodsTests >> createQuery [
	^ ClyOverriddenMethods from: ClyClassScope of: ClyAbstractClassExample in: environment
]

{ #category : #running }
ClyOverriddenMethodsTests >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]

{ #category : #tests }
ClyOverriddenMethodsTests >> testCheckIfEmpty [
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
ClyOverriddenMethodsTests >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyAbstractClassExample >> #abstractMethod1. 
		ClyAbstractClassExample >> #abstractMethod2.
		ClyAbstractClassExample >> #overriddenMethod}.
	
	self assert: foundSelectors equals: #(abstractMethod1 overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsTests >> testIsAffectedByChangeOfOwnMethodWhichIsNotOverridden [
	query := ClyOverriddenMethods 
		from: ClyClassScope of: ClyClassWhichInheritsAbstractClass in: environment.
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass >>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsTests >> testIsAffectedByChangeOfOwnMethodWhichIsOverridden [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassExample>>#overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsTests >> testIsAffectedByChangeOfSubclassMethodWhichOverridesSuperclass [
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass>>#overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsTests >> testNotAffectedByChangeOfSubclassMethodWhichNotOverridesSuperclass [
	
	self deny: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsTests >> testNotSelectsNotOverriddenMethod [

	self deny: (query selectsMethod: ClyClassWhichInheritsAbstractClass>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsTests >> testSelectsOverriddenMethod [

	self assert: (query selectsMethod: ClyAbstractClassExample>>#overriddenMethod)
]
