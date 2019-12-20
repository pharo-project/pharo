"
SUnit tests for class WikiStyleHelpBuilder
"
Class {
	#name : #WikiStyleHelpBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'HelpSystem-Tests-Builders'
}

{ #category : #accessing }
WikiStyleHelpBuilderTest >> defaultTestClass [
	^WikiStyleHelpBuilder
]

{ #category : #running }
WikiStyleHelpBuilderTest >> setUp [
	super setUp.
	builder := self defaultTestClass new
]

{ #category : #testing }
WikiStyleHelpBuilderTest >> testDefaultSyntaxAlwaysPier [
	self assert: builder defaultSyntax identicalTo: #pier
]

{ #category : #testing }
WikiStyleHelpBuilderTest >> testExtractHelpContentsFromMarkedString [
	| content |
	content := builder extractHelpContentFromString: 'outer<help>inner</help>outer'.
	self assert: content equals: 'inner'
]
