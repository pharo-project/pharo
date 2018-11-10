"
I am a command to switch query browser to hierarchically sorted items list.

In this mode items is grouped by defining class and shown in class hierarchy tree sorted by name
"
Class {
	#name : #ClySwitchToHierarchicalQueryResultCommand,
	#superclass : #ClySwitchQueryResultCommand,
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Controlling'
}

{ #category : #execution }
ClySwitchToHierarchicalQueryResultCommand >> requiredQueryResult [
	^ClyHierarchicalSystemItems new
]
