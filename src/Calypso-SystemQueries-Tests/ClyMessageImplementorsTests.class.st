Class {
	#name : #ClyMessageImplementorsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #'methods for tests' }
ClyMessageImplementorsTests class >> clyMessageImplementor1 [
]

{ #category : #'methods for tests' }
ClyMessageImplementorsTests >> clyMessageImplementor1 [
]

{ #category : #running }
ClyMessageImplementorsTests >> createQuery [
	^ ClyMessageImplementors
		of: self targetSelector
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #'methods for tests' }
ClyMessageImplementorsTests >> messageImplementor2 [
]

{ #category : #running }
ClyMessageImplementorsTests >> targetSelector [
	^#clyMessageImplementor1
]

{ #category : #tests }
ClyMessageImplementorsTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope of: (self class >> self targetSelector) in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: (self class >> #messageImplementor2) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyMessageImplementorsTests >> testComparison [

	self 
		assert: (ClyMessageImplementors of: #message1) 
		equals: (ClyMessageImplementors of: #message1).
	
	self 
		assert: (ClyMessageImplementors of: #message1) hash 
		equals: (ClyMessageImplementors of: #message1) hash.
		
	self deny: (ClyMessageImplementors of: #message1) = (ClyMessageImplementors of: #message2)
]

{ #category : #tests }
ClyMessageImplementorsTests >> testFromClassScope [
	
	self queryFromScope: ClyBothMetaLevelClassScope of: self class.
	
	self assert: resultItems size equals: 2.	
	self assert: (resultItems identityIncludes: (self class >> self targetSelector)).
	self assert: (resultItems identityIncludes: (self class classSide >> self targetSelector)).
]

{ #category : #tests }
ClyMessageImplementorsTests >> testFromMethodScope [
	
	self 
		queryFromScope: ClyMethodScope 
		ofAll: { self class >> self targetSelector. self class >> #messageImplementor2 }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> self targetSelector)
]

{ #category : #tests }
ClyMessageImplementorsTests >> testFromPackageScope [
	
	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 3.	
	self assert: (resultItems identityIncludes: self class >> self targetSelector).
	self assert: (resultItems identityIncludes: self class classSide >> self targetSelector).
	self assert: (resultItems identityIncludes: self class superclass >> self targetSelector).
]

{ #category : #tests }
ClyMessageImplementorsTests >> testFromSystemScope [

	self queryFromScope: ClySystemEnvironmentScope of: ClySystemEnvironment currentImage.
	
	self assert: resultItems size equals: 3.	
	self assert: (resultItems identityIncludes: self class >> self targetSelector).
	self assert: (resultItems identityIncludes: self class classSide >> self targetSelector).
	self assert: (resultItems identityIncludes: self class superclass >> self targetSelector).
]
