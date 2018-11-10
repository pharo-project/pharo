Class {
	#name : #ClyConstantMethodQueryTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyConstantMethodQueryTests >> createQuery [
	^ (ClyConstantMethodQuery
		with: {self class >> #method1. self class >> #method2})
			scope: (ClyScopeExample emptyIn: environment)
]

{ #category : #running }
ClyConstantMethodQueryTests >> method1 [
	^#method1
]

{ #category : #running }
ClyConstantMethodQueryTests >> method2 [
	^#method2
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testCheckIfEmpty [
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testComparison [

	self 
		assert: (ClyConstantMethodQuery with: {self class >> #method1}) 
		equals: (ClyConstantMethodQuery with: {self class >> #method1}).
	
	self 
		assert: (ClyConstantMethodQuery with: {self class >> #method1}) hash 
		equals: (ClyConstantMethodQuery with: {self class >> #method1}) hash.
		
	self deny: (ClyConstantMethodQuery with: {self class >> #method1}) = (ClyConstantMethodQuery with: {self class >> #method2})
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testConvertingToAsyncQuery [

	| convertedQuery |
	convertedQuery := query async.
	
	self assert: convertedQuery == query
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testConvertingToSemiAsyncQuery [

	| convertedQuery |
	convertedQuery := query semiAsync.
	
	self assert: convertedQuery == query
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testEmptyWhenNoMethods [
	query scope: (ClyMethodScope of: (self class >> #method1) in: environment).
		
	query methods: #().
	
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testFromPackageScope [

	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 2.	
	self assert: (resultItems identityIncludes: self class >> #method1).
	self assert: (resultItems identityIncludes: self class >> #method2)
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testFromSystemScope [

	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.
	
	self assert: resultItems size equals: 2.	
	self assert: (resultItems identityIncludes: self class >> #method1).
	self assert: (resultItems identityIncludes: self class >> #method2)
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testFromWrongClassScope [

	self queryFromScope: ClyClassScope of: self class superclass.
	
	self assert: resultItems isEmpty
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testIsEmptyFromEmptyMethodScope [
	"Constant query do not depends on scope. So we redefine this method"
	<expectedFailure>
	super testIsEmptyFromEmptyMethodScope
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testNotEmptyFromEmptyMethodScope [
	query scope: (ClyMethodScope emptyIn: environment).
	
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyConstantMethodQueryTests >> testNotSelectsMethodOnWhichItIsBased [

	self deny: (query selectsMethod: Point >> #x) 

]

{ #category : #tests }
ClyConstantMethodQueryTests >> testSelectsMethodOnWhichItIsBased [

	self assert: (query selectsMethod: self class >> #method1) 

]
