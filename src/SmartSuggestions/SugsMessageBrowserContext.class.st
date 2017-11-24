"
I know how to use a Text model that came from a SmalltalkEditor and extract the information needed for the suggestions :)
"
Class {
	#name : #SugsMessageBrowserContext,
	#superclass : #SugsAbstractContext,
	#category : #'SmartSuggestions-Menu'
}

{ #category : #selection }
SugsMessageBrowserContext >> selectedClass [
	^model selectedBehavior
]

{ #category : #selection }
SugsMessageBrowserContext >> selectedMethod [
	^model owner selectedMessage.
]

{ #category : #refactoring }
SugsMessageBrowserContext >> sourceTextArea [
	^ model widget.
]

{ #category : #tools }
SugsMessageBrowserContext >> tools [
	^ model systemNavigation
]
