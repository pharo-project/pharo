Class {
	#name : #ClyAllMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllMethodsTests >> createQuery [
	^ ClyAllMethods from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyAllMethodsTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClyClass1FromP1 >> #tag1Method1) in: environment).
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyAllMethodsTests >> testFromSingleClass [

	self queryFromScope: ClyClassScope of: ClyClass1FromP1.
	
	self assert: foundSelectors equals: #(#instanceSideVar1ReaderMethod #instanceSideVar1WriterMethod #superclassTag1Method #tag1Method1)
]
