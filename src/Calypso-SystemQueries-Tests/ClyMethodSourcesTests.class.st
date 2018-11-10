Class {
	#name : #ClyMethodSourcesTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyMethodSourcesTests >> createQuery [
	^ ClyMethodSources
		withString: self expectedSourceCodeSubstring
		from: (ClyClassScope of: self class in: environment)
]

{ #category : #'example methods' }
ClyMethodSourcesTests >> expectedSourceCodeSubstring [
	^'it should be in the source code of ', #expectedMethod
]

{ #category : #'example methods' }
ClyMethodSourcesTests >> methodWithExpectedStringInSources [
	"it should be in the source code of expectedMethod"
	^self printString
]

{ #category : #tests }
ClyMethodSourcesTests >> testCheckIfEmpty [
	query scope: (ClyClassScope of: self class in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: self class superclass in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyMethodSourcesTests >> testComparison [

	self 
		assert: (ClyMethodSources withString: 'test substring') 
		equals: (ClyMethodSources withString: 'test ', 'substring').
		
	self 
		assert: (ClyMethodSources withString: 'test substring') hash
		equals: (ClyMethodSources withString: 'test ', 'substring') hash.
		
	self deny: (ClyMethodSources withString: 'test substring')
					 = (ClyMethodSources withString: 'another substring')
]

{ #category : #tests }
ClyMethodSourcesTests >> testExpectedMethodShouldIncludeExpectedString [

	self assert: ((self class >> #methodWithExpectedStringInSources) sourceCode 
		includesSubstring: self expectedSourceCodeSubstring caseSensitive: false)
]

{ #category : #tests }
ClyMethodSourcesTests >> testFromClassScope [

	self queryFromScope: ClyClassScope of: self class.
	
	self assert: resultItems size equals: 1.
	
	self assert: resultItems first equals: (self class >> #methodWithExpectedStringInSources)
]
