"
A CBCritiqueToolbarTest is a test class for testing the behaivor of CBCritiqueToolbar
"
Class {
	#name : #CBCritiqueToolbarTest,
	#superclass : #TestCase,
	#instVars : [
		'critiqueToolbar'
	],
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #running }
CBCritiqueToolbarTest >> setUp [

	super setUp.
	critiqueToolbar := CBCritiqueToolbar new.
]

{ #category : #initialization }
CBCritiqueToolbarTest >> testInitialize [

	self assert: critiqueToolbar criticHolder value isNil.
	self assert: critiqueToolbar ruleHolder value isNil.
	
]

{ #category : #initialization }
CBCritiqueToolbarTest >> testOpen [

	| window |
	window := CBCritiqueToolbar open.
	self assert: window isBuilt.
	window close
]
