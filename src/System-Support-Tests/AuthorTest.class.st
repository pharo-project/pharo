"
SUnit tests for Author
"
Class {
	#name : 'AuthorTest',
	#superclass : 'TestCase',
	#category : 'System-Support-Tests-Authoring',
	#package : 'System-Support-Tests',
	#tag : 'Authoring'
}

{ #category : 'tests' }
AuthorTest >> testUniqueness [
	self should: [ Author new ] raise: Error
]
