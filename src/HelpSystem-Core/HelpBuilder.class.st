"
This is an utility class that builds the books for a help system.

Instance Variables
	rootTopics:		<OrderedCollection>

rootTopics
	- a collection of books 

"
Class {
	#name : #HelpBuilder,
	#superclass : #Model,
	#instVars : [
		'topicToBuild',
		'rootToBuildFrom'
	],
	#category : 'HelpSystem-Core-Builders'
}

{ #category : #building }
HelpBuilder class >> buildHelpTopicFrom: aHelpTopicDescription [
	"Start building from the given help topic description"
	
	^(self new)
		rootToBuildFrom: aHelpTopicDescription;
		build;
		topicToBuild 
]

{ #category : #building }
HelpBuilder >> build [
	self subclassResponsibility 

]

{ #category : #initialization }
HelpBuilder >> initialize [ 
	"Initializes the receiver"
	
	super initialize.
	topicToBuild := self topicClass new.
	 
]

{ #category : #accessing }
HelpBuilder >> rootToBuildFrom: anObject [
	rootToBuildFrom := anObject
]

{ #category : #'private accessing' }
HelpBuilder >> topicClass [
	^HelpTopic
]

{ #category : #accessing }
HelpBuilder >> topicToBuild [
	^topicToBuild
]
