Class {
	#name : #AuthorTest,
	#superclass : #TestCase,
	#instVars : [
		'author'
	],
	#category : 'Tests-System'
}

{ #category : #tests }
AuthorTest >> testUniqueness [
	self should: [ Author new ] raise: Error.
]
