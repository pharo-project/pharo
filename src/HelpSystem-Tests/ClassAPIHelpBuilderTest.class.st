"
SUnit tests for ClassAPIHelpBuilder
"
Class {
	#name : #ClassAPIHelpBuilderTest,
	#superclass : #TestCase,
	#category : #'HelpSystem-Tests-Builders'
}

{ #category : #testing }
ClassAPIHelpBuilderTest >> testBuildingTraits [
	| topic |
	topic := ClassAPIHelpBuilder buildHelpTopicFrom: TSortable.
	self assert: topic subtopics size equals: 2.
	self assert: topic subtopics first title equals: 'Instance side'.
	self assert: topic subtopics last title equals: 'Class side'
]

{ #category : #testing }
ClassAPIHelpBuilderTest >> testDefaultBuilding [
	| topic |
	topic := ClassAPIHelpBuilder buildHelpTopicFrom: Integer.
	self assert: topic subtopics size equals: 2.
	self assert: topic subtopics first title equals: 'Instance side'.
	self assert: topic subtopics last title equals: 'Class side'
]

{ #category : #testing }
ClassAPIHelpBuilderTest >> testMethodsButNoSubclasses [
	| topic |
	topic := ClassAPIHelpBuilder buildHierarchicalHelpTopicFrom: Integer withSubclasses: false withMethods: true.
	self assert: topic subtopics size equals: 2.
	self assert: topic subtopics first title equals: 'Instance side'.
	self assert: topic subtopics last title equals: 'Class side'
]
