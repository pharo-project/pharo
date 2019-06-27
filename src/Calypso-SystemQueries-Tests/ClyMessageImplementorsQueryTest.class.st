Class {
	#name : #ClyMessageImplementorsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #'methods for tests' }
ClyMessageImplementorsQueryTest class >> clyMessageImplementor1 [
]

{ #category : #'methods for tests' }
ClyMessageImplementorsQueryTest >> clyMessageImplementor1 [
]

{ #category : #running }
ClyMessageImplementorsQueryTest >> createQuery [
	^ ClyMessageImplementorsQuery
		of: self targetSelector
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #'methods for tests' }
ClyMessageImplementorsQueryTest >> messageImplementor2 [
]

{ #category : #running }
ClyMessageImplementorsQueryTest >> targetSelector [
	^#clyMessageImplementor1
]

{ #category : #tests }
ClyMessageImplementorsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope of: (self class >> self targetSelector) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: (self class >> #messageImplementor2) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyMessageImplementorsQueryTest >> testComparison [

	self 
		assert: (ClyMessageImplementorsQuery of: #message1) 
		equals: (ClyMessageImplementorsQuery of: #message1).
	
	self 
		assert: (ClyMessageImplementorsQuery of: #message1) hash 
		equals: (ClyMessageImplementorsQuery of: #message1) hash.
		
	self deny: (ClyMessageImplementorsQuery of: #message1) = (ClyMessageImplementorsQuery of: #message2)
]

{ #category : #tests }
ClyMessageImplementorsQueryTest >> testFromClassScope [
	
	self queryFromScope: ClyBothMetaLevelClassScope of: self class.
	
	self assert: resultItems size equals: 2.	
	self assert: (resultItems identityIncludes: (self class >> self targetSelector)).
	self assert: (resultItems identityIncludes: (self class classSide >> self targetSelector)).
]

{ #category : #tests }
ClyMessageImplementorsQueryTest >> testFromMethodScope [
	
	self 
		queryFromScope: ClyMethodScope 
		ofAll: { self class >> self targetSelector. self class >> #messageImplementor2 }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> self targetSelector)
]

{ #category : #tests }
ClyMessageImplementorsQueryTest >> testFromPackageScope [
	
	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 3.	
	self assert: (resultItems identityIncludes: self class >> self targetSelector).
	self assert: (resultItems identityIncludes: self class classSide >> self targetSelector).
	self assert: (resultItems identityIncludes: self class superclass >> self targetSelector).
]

{ #category : #tests }
ClyMessageImplementorsQueryTest >> testFromSystemScope [

	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.
	
	self assert: resultItems size equals: 3.	
	self assert: (resultItems identityIncludes: self class >> self targetSelector).
	self assert: (resultItems identityIncludes: self class classSide >> self targetSelector).
	self assert: (resultItems identityIncludes: self class superclass >> self targetSelector).
]
