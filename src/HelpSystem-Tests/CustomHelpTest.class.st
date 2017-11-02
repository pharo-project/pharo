"
SUnit tests for class CustomHelp

"
Class {
	#name : #CustomHelpTest,
	#superclass : #TestCase,
	#category : 'HelpSystem-Tests-Core-Utilities'
}

{ #category : #testing }
CustomHelpTest >> testOpenBrowserOnThisHelpType [
	| browser |
	browser := HelpBrowser openOn: CustomHelp.
	World doOneCycleNow.
	browser close
]
