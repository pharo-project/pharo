"
I am a test case which describes the ClyClassCommentQuery.
It should find all classes which comments includes the given string.
In tests I use following substring:
	it should be in the comment of this class to test class comment query
which should be found
"
Class {
	#name : #ClyClassCommentsTests,
	#superclass : #ClyClassQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyClassCommentsTests >> createQuery [
	^ ClyClassComments
		withString: self expectedClassCommentSubstring
		from: (ClyPackageScope of: Object package in: environment)
]

{ #category : #running }
ClyClassCommentsTests >> expectedClassCommentSubstring [
	^'it should be in the comment of this class to test class comment query'
]

{ #category : #tests }
ClyClassCommentsTests >> testCheckIfEmpty [
	query scope: (ClyClassScope of: self class in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: self class superclass in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyClassCommentsTests >> testComparison [

	self 
		assert: (ClyClassComments withString: 'test substring') 
		equals: (ClyClassComments withString: 'test ', 'substring').
		
	self 
		assert: (ClyClassComments withString: 'test substring') hash
		equals: (ClyClassComments withString: 'test ', 'substring') hash.
		
	self deny: (ClyClassComments withString: 'test substring')
					 = (ClyClassComments withString: 'another substring')
]

{ #category : #tests }
ClyClassCommentsTests >> testFromClassScope [

	self queryFromScope: ClyClassScope ofAll: { self class. self class superclass }.
	
	self assert: resultItems size equals: 1.
	
	self assert: resultItems first equals: (ClyClassComment of: self class)
]

{ #category : #tests }
ClyClassCommentsTests >> testFromClassWithoutCommentWhenPatternSatisfiesCommentTemplate [

	| noCommentClass substring |
	noCommentClass := self class superclass.
	self deny: noCommentClass hasComment.
	substring := noCommentClass comment copyFrom: 4 to: 30.
	
	query pattern: (ClySubstringPattern with: substring).	
	self queryFromScope: ClyClassScope of: noCommentClass.
	
	self assert: resultItems isEmpty
]

{ #category : #tests }
ClyClassCommentsTests >> testThisClassShouldIncludeExpectedComment [

	self assert: (self class comment 
		includesSubstring: self expectedClassCommentSubstring caseSensitive: false)
]
