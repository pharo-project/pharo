Class {
	#name : #ClyOverriddenMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverriddenMethodsQueryTest >> createQuery [
	^ ClyOverriddenMethodsQuery from: ClyClassScope of: ClyAbstractClassMock in: environment
]

{ #category : #running }
ClyOverriddenMethodsQueryTest >> setUpEnvironment [
	super setUpEnvironment.
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope 
		of: (ClyAbstractClassMock >> #abstractMethod1)	in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyAbstractClassMock >> #overriddenMethod) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyAbstractClassMock >> #abstractMethod2) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyAbstractClassMock >> #abstractMethod1. 
		ClyAbstractClassMock >> #abstractMethod2.
		ClyAbstractClassMock >> #overriddenMethod}.
	
	self assert: foundSelectors equals: #(abstractMethod1 overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testFromThreeMethodsWhenImplementorsCacheShouldBeUsed [
	| plugin |
	plugin := environment getPlugin: ClyInheritanceAnalysisEnvironmentPlugin.
	plugin littleHierarchyMaxSize: -1. 
	"This parameter forces plugin to build and use the cache for analysis"
	
	self queryFromScope: ClyMethodScope ofAll: {
		ClyAbstractClassMock >> #abstractMethod1. 
		ClyAbstractClassMock >> #abstractMethod2.
		ClyAbstractClassMock >> #overriddenMethod}.
	
	self assert: foundSelectors equals: #(abstractMethod1 overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testIsAffectedByChangeOfOwnMethodWhichIsNotOverridden [
	query := ClyOverriddenMethodsQuery 
		from: ClyClassScope of: ClyClassWhichInheritsAbstractClassMock in: environment.
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClassMock >>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testIsAffectedByChangeOfOwnMethodWhichIsOverridden [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassMock>>#overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testIsAffectedByChangeOfSubclassMethodWhichOverridesSuperclass [
	
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClassMock>>#overriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testNotAffectedByChangeOfSubclassMethodWhichNotOverridesSuperclass [
	
	self deny: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClassMock>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testNotSelectsNotOverriddenMethod [

	self deny: (query selectsMethod: ClyClassWhichInheritsAbstractClassMock>>#notOverriddenMethod)
]

{ #category : #tests }
ClyOverriddenMethodsQueryTest >> testSelectsOverriddenMethod [

	self assert: (query selectsMethod: ClyAbstractClassMock>>#overriddenMethod)
]
