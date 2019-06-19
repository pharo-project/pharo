Class {
	#name : #ClyFFIMethodsTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-FFI-Queries-Tests'
}

{ #category : #running }
ClyFFIMethodsTest >> createQuery [
	^ClyFFIMethodQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyFFIMethodsTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClyClassWithFFIMethods >> #ffiMethod)
		in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWithFFIMethods >> #notFFIMethod)
		in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyFFIMethodsTest >> testFromTwoMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWithFFIMethods >> #ffiMethod. 
		ClyClassWithFFIMethods >> #notFFIMethod}.
	
	self assert: foundSelectors sorted asArray equals: #(ffiMethod)
]

{ #category : #tests }
ClyFFIMethodsTest >> testNotSelectsNotFFIMethod [

	self deny: (query selectsMethod: ClyClassWithFFIMethods >> #notFFIMethod)
]

{ #category : #tests }
ClyFFIMethodsTest >> testSelectsFFIMethod [

	self assert: (query selectsMethod: ClyClassWithFFIMethods >> #ffiMethod)
]
