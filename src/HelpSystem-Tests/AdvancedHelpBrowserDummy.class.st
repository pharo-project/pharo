"
This is a dummy for a custom Help browser that can be registered as a replacement for the HelpBrowser class.

Instance Variables
	rootTopic:		<HelpTopic>

rootTopic
	- The root help topic

"
Class {
	#name : #AdvancedHelpBrowserDummy,
	#superclass : #Object,
	#instVars : [
		'rootTopic',
		'isOpen'
	],
	#category : 'HelpSystem-Tests-Core-UI'
}

{ #category : #initialization }
AdvancedHelpBrowserDummy >> initialize [
	isOpen := false
]

{ #category : #testing }
AdvancedHelpBrowserDummy >> isOpen [
	^isOpen
]

{ #category : #mocking }
AdvancedHelpBrowserDummy >> open [
	isOpen := true
]

{ #category : #mocking }
AdvancedHelpBrowserDummy >> rootTopic [
	^rootTopic
]

{ #category : #mocking }
AdvancedHelpBrowserDummy >> rootTopic: aTopic [
	rootTopic := aTopic
]
