"
SUnit tests for the help browser
"
Class {
	#name : #HelpBrowserTest,
	#superclass : #TestCase,
	#category : #'HelpSystem-Tests-Core-UI'
}

{ #category : #accessing }
HelpBrowserTest >> defaultTestClass [
	^HelpBrowser
]

{ #category : #testing }
HelpBrowserTest >> testDefaultHelpBrowser [
	| current replacement instance |
	current := self defaultTestClass defaultHelpBrowser.
	replacement := AdvancedHelpBrowserDummy.
	[ self defaultTestClass defaultHelpBrowser: replacement.
	self assert: self defaultTestClass defaultHelpBrowser identicalTo: replacement.

	instance := self defaultTestClass open.
	self assert: instance rootTopic notNil.
	self assert: instance isOpen ]
		ensure: [ self defaultTestClass defaultHelpBrowser: current ]
]

{ #category : #testing }
HelpBrowserTest >> testDefaultHelpBrowserIsReplacable [
	| current replacement instance |
	"save the one that is registered"
	current := self defaultTestClass defaultHelpBrowser.
	replacement := AdvancedHelpBrowserDummy.
	[ self defaultTestClass defaultHelpBrowser: replacement.
	self assert: self defaultTestClass defaultHelpBrowser identicalTo: replacement.
	instance := self defaultTestClass open ]
		ensure: [ self defaultTestClass defaultHelpBrowser: current ]
]

{ #category : #testing }
HelpBrowserTest >> testLazyDefaultHelpBrowser [
	
	self assert: self defaultTestClass defaultHelpBrowser notNil
]

{ #category : #testing }
HelpBrowserTest >> testOpen [
	|browser|
	browser := self defaultTestClass open.
	self currentWorld doOneCycleNow. 
	browser close 


		
]
