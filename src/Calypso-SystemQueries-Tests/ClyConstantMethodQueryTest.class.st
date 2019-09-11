Class {
	#name : #ClyConstantMethodQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyConstantMethodQueryTest >> createQuery [
	^ (ClyConstantMethodQuery
		with: {self class >> #method1. self class >> #method2})
			scope: (ClyScopeExample emptyIn: environment)
]

{ #category : #running }
ClyConstantMethodQueryTest >> method1 [
	^#method1
]

{ #category : #running }
ClyConstantMethodQueryTest >> method2 [
	^#method2
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testCheckIfEmpty [
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testComparison [

	self 
		assert: (ClyConstantMethodQuery with: {self class >> #method1}) 
		equals: (ClyConstantMethodQuery with: {self class >> #method1}).
	
	self 
		assert: (ClyConstantMethodQuery with: {self class >> #method1}) hash 
		equals: (ClyConstantMethodQuery with: {self class >> #method1}) hash.
		
	self deny: (ClyConstantMethodQuery with: {self class >> #method1}) = (ClyConstantMethodQuery with: {self class >> #method2})
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testConvertingToAsyncQuery [

	| convertedQuery |
	convertedQuery := query async.
	
	self assert: convertedQuery == query
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testConvertingToSemiAsyncQuery [

	| convertedQuery |
	convertedQuery := query semiAsync.
	
	self assert: convertedQuery == query
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testEmptyWhenNoMethods [
	query scope: (ClyMethodScope of: (self class >> #method1) in: environment).
		
	query methods: #().
	
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testFromPackageScope [

	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 2.	
	self assert: (resultItems identityIncludes: self class >> #method1).
	self assert: (resultItems identityIncludes: self class >> #method2)
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testFromSystemScope [

	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.
	
	self assert: resultItems size equals: 2.	
	self assert: (resultItems identityIncludes: self class >> #method1).
	self assert: (resultItems identityIncludes: self class >> #method2)
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testFromWrongClassScope [

	self queryFromScope: ClyClassScope of: self class superclass.
	
	self assert: resultItems isEmpty
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testIsEmptyFromEmptyMethodScope [
	"Constant query do not depends on scope. So we redefine this method"
	<expectedFailure>
	super testIsEmptyFromEmptyMethodScope
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testNotEmptyFromEmptyMethodScope [
	query scope: (ClyMethodScope emptyIn: environment).
	
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyConstantMethodQueryTest >> testNotSelectsMethodOnWhichItIsBased [

	self deny: (query selectsMethod: Point >> #x) 

]

{ #category : #tests }
ClyConstantMethodQueryTest >> testSelectsMethodOnWhichItIsBased [

	self assert: (query selectsMethod: self class >> #method1) 

]
