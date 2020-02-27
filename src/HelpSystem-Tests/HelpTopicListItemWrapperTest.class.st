"
SUnit tests for class HelpTopicListItemWrapper
"
Class {
	#name : #HelpTopicListItemWrapperTest,
	#superclass : #TestCase,
	#category : #'HelpSystem-Tests-Core-UI'
}

{ #category : #accessing }
HelpTopicListItemWrapperTest >> defaultTestClass [ 
	^HelpTopicListItemWrapper 
	
]

{ #category : #testing }
HelpTopicListItemWrapperTest >> testDisplayLabel [
	| instance |
	instance := self defaultTestClass with: (HelpTopic named: 'My Topic').
	self assert: instance asString equals: 'My Topic'
]
