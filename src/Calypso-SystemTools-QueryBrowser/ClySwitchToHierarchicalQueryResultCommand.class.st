"
I am a command to switch query browser to hierarchically sorted items list.

In this mode items is grouped by defining class and shown in class hierarchy tree sorted by name
"
Class {
	#name : 'ClySwitchToHierarchicalQueryResultCommand',
	#superclass : 'ClySwitchQueryResultCommand',
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Controlling',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Controlling'
}

{ #category : 'execution' }
ClySwitchToHierarchicalQueryResultCommand >> executeRefactoring [
	self browser switchToHierarchicalResult
]

{ #category : 'testing' }
ClySwitchToHierarchicalQueryResultCommand >> isAppliedToBrowser [
	^self browser showsFlatResult not
]

{ #category : 'accessing' }
ClySwitchToHierarchicalQueryResultCommand >> name [
	^'Hier.'
]
