"
I am a command to spawn full browser from current browser state.
I simply ask browser #spawnFullBrowser.

By default I am available everywhere using cmd+b shortcut and ""Browse"" context menu item.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>

"
Class {
	#name : #ClySpawnFullBrowserCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Queries'
}

{ #category : #activation }
ClySpawnFullBrowserCommand class >> browserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClySystemBrowserContext 
]

{ #category : #activation }
ClySpawnFullBrowserCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $b meta for: ClySystemBrowserContext
]

{ #category : #testing }
ClySpawnFullBrowserCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext hasSelectedItems
]

{ #category : #activation }
ClySpawnFullBrowserCommand class >> fullBrowserDoubleClickMenuActivation [
	<classAnnotation>
	
	^CmdDoubleClickActivation for: ClyFullBrowserContext
]

{ #category : #accessing }
ClySpawnFullBrowserCommand >> defaultMenuIconName [
	^#glamorousBrowse
]

{ #category : #accessing }
ClySpawnFullBrowserCommand >> defaultMenuItemName [
	^'Browse'
]

{ #category : #execution }
ClySpawnFullBrowserCommand >> execute [

	browser spawnFullBrowser 
]

{ #category : #execution }
ClySpawnFullBrowserCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	browser := aToolContext browser
]
