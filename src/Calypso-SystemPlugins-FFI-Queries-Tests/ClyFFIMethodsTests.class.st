Class {
	#name : #ClyFFIMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-FFI-Queries-Tests'
}

{ #category : #running }
ClyFFIMethodsTests >> createQuery [
	^ClyFFIMethodQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyFFIMethodsTests >> testCheckIfEmpty [
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
ClyFFIMethodsTests >> testFromTwoMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWithFFIMethods >> #ffiMethod. 
		ClyClassWithFFIMethods >> #notFFIMethod}.
	
	self assert: foundSelectors sorted asArray equals: #(ffiMethod)
]

{ #category : #tests }
ClyFFIMethodsTests >> testNotSelectsNotFFIMethod [

	self deny: (query selectsMethod: ClyClassWithFFIMethods >> #notFFIMethod)
]

{ #category : #tests }
ClyFFIMethodsTests >> testSelectsFFIMethod [

	self assert: (query selectsMethod: ClyClassWithFFIMethods >> #ffiMethod)
]
