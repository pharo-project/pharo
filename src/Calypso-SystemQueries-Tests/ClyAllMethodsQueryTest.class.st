Class {
	#name : #ClyAllMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllMethodsQueryTest >> createQuery [
	^ ClyAllMethodsQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyAllMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClyClass1FromP1 >> #tag1Method1) in: environment).
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyAllMethodsQueryTest >> testFromSingleClass [

	self queryFromScope: ClyClassScope of: ClyClass1FromP1.
	
	self assert: foundSelectors equals: #(#instanceSideVar1ReaderMethod #instanceSideVar1WriterMethod #superclassTag1Method #tag1Method1)
]
