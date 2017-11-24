"
I know how to use a ProcessBrowser to extract the information needed for the suggestions
"
Class {
	#name : #SugsProcessBrowserContext,
	#superclass : #SugsAbstractContext,
	#category : #'SmartSuggestions-Menu'
}

{ #category : #selection }
SugsProcessBrowserContext >> selectedClass [
	^ model selectedClass
]

{ #category : #selection }
SugsProcessBrowserContext >> selectedMethod [
	^ model selectedMethod
]

{ #category : #refactoring }
SugsProcessBrowserContext >> sourceTextArea [
	| window |
	window := model dependents ifEmpty: [ nil ] ifNotEmpty: [ model dependents at: 1  ].
	
	^window submorphThat: [:each | each respondsTo: #selectionInterval] ifNone: [nil]
]
