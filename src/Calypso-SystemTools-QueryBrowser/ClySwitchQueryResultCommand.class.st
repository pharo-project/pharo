"
I am a base class for the commands which switch the required result in query browser.
It can be flat list or hierarchically sorted items.

My subclasses should implement class side method:

- requiredQueryResult
"
Class {
	#name : #ClySwitchQueryResultCommand,
	#superclass : #ClySwitchBrowserModeCommand,
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Controlling'
}

{ #category : #testing }
ClySwitchQueryResultCommand class >> isAbstract [
	^self = ClySwitchQueryResultCommand 
]

{ #category : #activation }
ClySwitchQueryResultCommand class >> queryBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyQueryResultToolbarGroup for: ClyQueryBrowserContext 
]
