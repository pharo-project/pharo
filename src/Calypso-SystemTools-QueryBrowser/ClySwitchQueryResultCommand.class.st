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

{ #category : #activation }
ClySwitchQueryResultCommand class >> queryBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyQueryResultToolbarGroup for: ClyQueryBrowserContext 
]

{ #category : #accessing }
ClySwitchQueryResultCommand >> defaultMenuItemName [
	^self requiredQueryResult shortName
]

{ #category : #execution }
ClySwitchQueryResultCommand >> execute [
	browser switchResultTo: self requiredQueryResult
]

{ #category : #testing }
ClySwitchQueryResultCommand >> isAppliedToBrowser [
	^browser showsQueryResultAs: self requiredQueryResult
]

{ #category : #accessing }
ClySwitchQueryResultCommand >> requiredQueryResult [
	self subclassResponsibility 
]
