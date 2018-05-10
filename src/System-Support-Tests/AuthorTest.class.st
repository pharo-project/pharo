"
SUnit tests for Author
"
Class {
	#name : #AuthorTest,
	#superclass : #TestCase,
	#instVars : [
		'author'
	],
	#category : #'System-Support-Tests'
}

{ #category : #tests }
AuthorTest >> testUniqueness [
	self should: [ Author new ] raise: Error
]
