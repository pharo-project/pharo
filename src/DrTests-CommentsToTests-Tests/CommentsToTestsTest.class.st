"
A DrTestsUITest is a test class for testing the behavior of DrTests-CommentsToTestsTest 
"
Class {
	#name : #CommentsToTestsTest,
	#superclass : #TestCase,
	#category : #'DrTests-CommentsToTests-Tests'
}

{ #category : #tests }
CommentsToTestsTest >> testErrorComment [
	| stringComment commentTestCase |
	stringComment := '(1+3)+6/0>>>4'.
	commentTestCase := CommentTestCase
		comment: stringComment
		class: self class
		selector: self selector.
	self
		assert: commentTestCase currentValue
		equals: commentTestCase expectedValue
]

{ #category : #tests }
CommentsToTestsTest >> testMultipleComments [
	| stringComment commentTestCase |
	stringComment := '(1+3)>>>4'.
	commentTestCase := CommentTestCase
		comment: stringComment
		class: self class
		selector: self selector.
	self
		assert: commentTestCase currentValue
		equals: commentTestCase expectedValue
]

{ #category : #tests }
CommentsToTestsTest >> testOneComment [
	| stringComment commentTestCase |
	stringComment := '(1+3)>>>4'.
	commentTestCase := CommentTestCase
		comment: stringComment
		class: self class
		selector: self selector.
	self
		assert: commentTestCase currentValue
		equals: commentTestCase expectedValue
]

{ #category : #tests }
CommentsToTestsTest >> testSimpleComment [
	| stringComment commentTestCase |
	stringComment := '(1+3)>>>4'.
	commentTestCase := CommentTestCase
		comment: stringComment
		class: self class
		selector: self selector.
	self
		assert: commentTestCase currentValue
		equals: commentTestCase expectedValue
]
