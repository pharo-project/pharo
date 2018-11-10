Class {
	#name : #ClyUnimplementedMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyUnimplementedMethodsTests >> createQuery [
	^ClyUnimplementedMethods 
		from: ClyClassScope 
		of: ClyClassWhichInheritsAbstractSubclass
		in: environment
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyAbstractSubclassExample in: environment).
	self deny: query checkEmptyResult.	"it has unimplemented #abstractMethod2"

	query scope: (ClyClassScope of: ClyAbstractClassExample in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testFromSingleClass [

	self executeQuery.
	
	self assert: foundSelectors equals: #(abstractMethod2 abstractSubclassMethod)

]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testIsAffectedByChangeOfAbstractMethodWhichIsImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassExample >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testIsAffectedByChangeOfAbstractMethodWhichIsNotImplemented [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractClassExample>>#abstractMethod2).
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassExample>>#abstractSubclassMethod)
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testIsAffectedByChangeOfMethodOfTargetClassWhichOverridesAbstractOne [
	query scope: (ClyClassScope	of: ClyClassWhichInheritsAbstractClass in: environment).
		
	self assert: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testIsAffectedByChangeOfNotAbstractMethodWhichImplementsAbstractOne [
	
	self assert: (query isAffectedByChangedMethod: ClyAbstractSubclassExample >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testIsEmptyForExplicitlyDefinedAbstractClass [

	query scope: (ClyClassScope of: ClyExplicitAbstractClassExample in: environment).

	self assert: query checkEmptyResult.
	
	self executeQuery.
	
	self assert: foundSelectors isEmpty
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testIsEmptyForExplicitlyDefinedAbstractMetaclass [

	query scope: (ClyClassScope of: ClyExplicitAbstractClassExample class in: environment).

	self assert: query checkEmptyResult.
	
	self executeQuery.
	
	self assert: foundSelectors isEmpty
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testIsEmptyFromEmptyClassScope [
	query scope: (ClyClassScope emptyIn: environment).
	
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testIsEmptyFromEmptyMethodScope [
	"This query only supports class, package and global scopes
	because it analyses classes and only then methods"
	<expectedFailure>
	super testIsEmptyFromEmptyMethodScope
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testNotAffectedByChangeOfAbstractMethodOfNotRelatedClass [
	
	self deny: (query isAffectedByChangedMethod: self class >> #createQuery)
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testNotAffectedByChangeOfNotAbstractMethod [
	query scope: (ClyClassScope of: ClyClassWhichInheritsAbstractClass in: environment).
	
	self deny: (query isAffectedByChangedMethod: ClyClassWhichInheritsAbstractClass >> #notOverriddenMethod)
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testNotSelectsAbstractMethodWhichIsImplemented [
	
	self deny: (query selectsMethod: ClyAbstractClassExample >> #abstractMethod1)
]

{ #category : #tests }
ClyUnimplementedMethodsTests >> testSelectsAbstractMethodWhichIsNotImplemented [

	self assert: (query selectsMethod: ClyAbstractClassExample>>#abstractMethod2).
	self assert: (query selectsMethod: ClyAbstractSubclassExample>>#abstractSubclassMethod)
]
