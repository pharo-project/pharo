Class {
	#name : #ClyUnimplementedMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyUnimplementedMethodsQueryTest >> createQuery [
	^ClyUnimplementedMethodsQuery 
		from: ClyClassScope 
		of: ClyClassWhichInheritsAbstractSubclassMock
		in: environment
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyAbstractSubclassMock in: environment).
	self deny: query checkEmptyResult.	"it has unimplemented #abstractMethod2"

	query scope: (ClyClassScope of: ClyAbstractClassMock in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testFromSingleClass [

	self executeQuery.
	
	self assert: foundSelectors equals: #(abstractMethod2 abstractSubclassMethod)

]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsAffectedByChangeOfAbstractMethodWhichIsImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassMock >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsAffectedByChangeOfAbstractMethodWhichIsNotImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassMock>>#abstractMethod2).
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassMock>>#abstractSubclassMethod)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsAffectedByChangeOfMethodOfTargetClassWhichOverridesAbstractOne [
	query scope: (ClyClassScope	of: ClyClassWhichInheritsAbstractClassMock in: environment).
		
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClassMock >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsAffectedByChangeOfNotAbstractMethodWhichImplementsAbstractOne [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassMock >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsEmptyForExplicitlyDefinedAbstractClass [

	query scope: (ClyClassScope of: ClyExplicitAbstractClassExampleMock in: environment).

	self assert: query checkEmptyResult.
	
	self executeQuery.
	
	self assert: foundSelectors isEmpty
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsEmptyForExplicitlyDefinedAbstractMetaclass [

	query scope: (ClyClassScope of: ClyExplicitAbstractClassExampleMock class in: environment).

	self assert: query checkEmptyResult.
	
	self executeQuery.
	
	self assert: foundSelectors isEmpty
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsEmptyFromEmptyClassScope [
	query scope: (ClyClassScope emptyIn: environment).
	
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsEmptyFromEmptyMethodScope [
	"This query only supports class, package and global scopes
	because it analyses classes and only then methods"
	<expectedFailure>
	super testIsEmptyFromEmptyMethodScope
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testNotAffectedByChangeOfAbstractMethodOfNotRelatedClass [
	
	self deny: (query isAffectedByChangedMethod: self class >> #createQuery)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testNotAffectedByChangeOfNotAbstractMethod [
	query scope: (ClyClassScope of: ClyClassWhichInheritsAbstractClassMock in: environment).
	
	self deny: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClassMock >> #notOverriddenMethod)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testNotSelectsAbstractMethodWhichIsImplemented [
	
	self deny: (query selectsMethod: ClyAbstractClassMock >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testSelectsAbstractMethodWhichIsNotImplemented [

	self assert: (query selectsMethod: ClyAbstractClassMock>>#abstractMethod2).
	self assert: (query selectsMethod: ClyAbstractSubclassMock>>#abstractSubclassMethod)
]
