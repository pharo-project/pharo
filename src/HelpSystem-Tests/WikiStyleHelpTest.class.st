"
SUnit tests for WikiStyleHelp
"
Class {
	#name : #WikiStyleHelpTest,
	#superclass : #TestCase,
	#category : #HelpSystem-Tests-Core-Utilities
}

{ #category : #testing }
WikiStyleHelpTest >> testOpenBrowserOnThisHelpType [
	| browser |
	browser := HelpBrowser openOn: WikiStyleHelp.
	World doOneCycleNow.
	browser close
]
