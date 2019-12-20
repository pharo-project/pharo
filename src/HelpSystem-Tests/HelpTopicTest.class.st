"
SUnit tests for HelpTopic class
"
Class {
	#name : #HelpTopicTest,
	#superclass : #TestCase,
	#instVars : [
		'topic'
	],
	#category : #'HelpSystem-Tests-Core-Model'
}

{ #category : #accessing }
HelpTopicTest >> defaultTestClass [
	^HelpTopic 
]

{ #category : #running }
HelpTopicTest >> setUp [
	super setUp.
	topic := self defaultTestClass new.
]

{ #category : #testing }
HelpTopicTest >> testAddingSubtopic [
	| subtopic returned |
	subtopic := self defaultTestClass named: 'Subtopic'.
	returned := topic addSubtopic: subtopic.
	self assert: returned identicalTo: subtopic.
	self assert: (topic subtopics includes: subtopic)
]

{ #category : #testing }
HelpTopicTest >> testInitialization [
	self assert: topic title equals: 'Unnamed Topic'.
	self assertEmpty: topic key.
	self assertEmpty: topic contents
]

{ #category : #testing }
HelpTopicTest >> testInstanceCreation [
	| instance |
	instance := self defaultTestClass named: 'My Topic'.
	self assert: instance title equals: 'My Topic'
]

{ #category : #testing }
HelpTopicTest >> testSortOrder [
	| a b c sorted |
	a := self defaultTestClass named: 'A'.
	b := self defaultTestClass named: 'B'.
	c := self defaultTestClass named: 'C'.
	sorted := (OrderedCollection with: b with: c with: a) asSortedCollection.
	self assert: sorted first equals: a.
	self assert: sorted last equals: c
]

{ #category : #testing }
HelpTopicTest >> testSubtopicOwnership [
	"Test that when a subtopic is added to a topic then owner of the subtopic is the topic"

	| subtopic owner |
	owner := self defaultTestClass named: 'I am the owner'.
	subtopic := self defaultTestClass named: 'I am the subtopic'.

	owner addSubtopic: subtopic.

	self assert: subtopic owner identicalTo: owner
]
