Class {
	#name : #ClyMethodSourcesQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyMethodSourcesQueryTest >> createQuery [
	^ ClyMethodSourcesQuery
		withString: self expectedSourceCodeSubstring
		from: (ClyClassScope of: self class in: environment)
]

{ #category : #'example methods' }
ClyMethodSourcesQueryTest >> expectedSourceCodeSubstring [
	^'it should be in the source code of ', #expectedMethod
]

{ #category : #'example methods' }
ClyMethodSourcesQueryTest >> methodWithExpectedStringInSources [
	"it should be in the source code of expectedMethod"
	^self printString
]

{ #category : #tests }
ClyMethodSourcesQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: self class in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: self class superclass in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyMethodSourcesQueryTest >> testComparison [

	self 
		assert: (ClyMethodSourcesQuery withString: 'test substring') 
		equals: (ClyMethodSourcesQuery withString: 'test ', 'substring').
		
	self 
		assert: (ClyMethodSourcesQuery withString: 'test substring') hash
		equals: (ClyMethodSourcesQuery withString: 'test ', 'substring') hash.
		
	self deny: (ClyMethodSourcesQuery withString: 'test substring')
					 = (ClyMethodSourcesQuery withString: 'another substring')
]

{ #category : #tests }
ClyMethodSourcesQueryTest >> testExpectedMethodShouldIncludeExpectedString [

	self assert: ((self class >> #methodWithExpectedStringInSources) sourceCode 
		includesSubstring: self expectedSourceCodeSubstring caseSensitive: false)
]

{ #category : #tests }
ClyMethodSourcesQueryTest >> testFromClassScope [

	self queryFromScope: ClyClassScope of: self class.
	
	self assert: resultItems size equals: 1.
	
	self assert: resultItems first equals: (self class >> #methodWithExpectedStringInSources)
]
