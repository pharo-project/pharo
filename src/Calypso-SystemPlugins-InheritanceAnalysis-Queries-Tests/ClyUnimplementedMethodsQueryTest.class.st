Class {
	#name : #ClyUnimplementedMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyUnimplementedMethodsQueryTest >> createQuery [
	^ClyUnimplementedMethodsQuery 
		from: ClyClassScope 
		of: ClyClassWhichInheritsAbstractSubclass
		in: environment
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyAbstractSubclassExample in: environment).
	self deny: query checkEmptyResult.	"it has unimplemented #abstractMethod2"

	query scope: (ClyClassScope of: ClyAbstractClassExample in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testFromSingleClass [

	self executeQuery.
	
	self assert: foundSelectors equals: #(abstractMethod2 abstractSubclassMethod)

]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsAffectedByChangeOfAbstractMethodWhichIsImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassExample >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsAffectedByChangeOfAbstractMethodWhichIsNotImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassExample>>#abstractMethod2).
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassExample>>#abstractSubclassMethod)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsAffectedByChangeOfMethodOfTargetClassWhichOverridesAbstractOne [
	query scope: (ClyClassScope	of: ClyClassWhichInheritsAbstractClass in: environment).
		
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsAffectedByChangeOfNotAbstractMethodWhichImplementsAbstractOne [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassExample >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsEmptyForExplicitlyDefinedAbstractClass [

	query scope: (ClyClassScope of: ClyExplicitAbstractClassExample in: environment).

	self assert: query checkEmptyResult.
	
	self executeQuery.
	
	self assert: foundSelectors isEmpty
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testIsEmptyForExplicitlyDefinedAbstractMetaclass [

	query scope: (ClyClassScope of: ClyExplicitAbstractClassExample class in: environment).

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
	query scope: (ClyClassScope of: ClyClassWhichInheritsAbstractClass in: environment).
	
	self deny: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass >> #notOverriddenMethod)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testNotSelectsAbstractMethodWhichIsImplemented [
	
	self deny: (query selectsMethod: ClyAbstractClassExample >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsQueryTest >> testSelectsAbstractMethodWhichIsNotImplemented [

	self assert: (query selectsMethod: ClyAbstractClassExample>>#abstractMethod2).
	self assert: (query selectsMethod: ClyAbstractSubclassExample>>#abstractSubclassMethod)
]
