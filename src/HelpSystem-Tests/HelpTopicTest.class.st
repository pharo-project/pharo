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

	|subtopic returned|
	subtopic := self defaultTestClass named: 'Subtopic'.
	returned := topic addSubtopic: subtopic.
	self assert: returned == subtopic.
	self assert: (topic subtopics includes: subtopic) 
]

{ #category : #testing }
HelpTopicTest >> testInitialization [

	self assert: topic title = 'Unnamed Topic'.
	self assert: topic key isEmpty.
	self assert: topic contents isEmpty 
]

{ #category : #testing }
HelpTopicTest >> testInstanceCreation [

	|instance|
	instance := self defaultTestClass named: 'My Topic'.
	self assert: instance title = 'My Topic'.

]

{ #category : #testing }
HelpTopicTest >> testSortOrder [

	|a b c sorted |
	a := self defaultTestClass named: 'A'.
	b := self defaultTestClass named: 'B'.
	c := self defaultTestClass named: 'C'.
	sorted := (OrderedCollection with: b with: c with: a) asSortedCollection.
	self assert: sorted first = a.
	self assert: sorted last = c.
	
]

{ #category : #testing }
HelpTopicTest >> testSubtopicOwnership [
"Test that when a subtopic is added to a topic then owner of the subtopic is the topic"
	|subtopic owner|
	owner := self defaultTestClass named:  'I am the owner'.
	subtopic := self defaultTestClass named: 'I am the subtopic'.
	
	owner addSubtopic: subtopic.
	
	self assert: (subtopic owner == owner).

]
