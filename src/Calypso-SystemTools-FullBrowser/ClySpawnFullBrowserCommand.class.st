"
I am a command to spawn full browser from current browser state.
I simply ask browser #spawnFullBrowser.

By default I am available everywhere using cmd+b shortcut and ""Browse"" context menu item.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>

"
Class {
	#name : 'ClySpawnFullBrowserCommand',
	#superclass : 'ClyBrowserCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'testing' }
ClySpawnFullBrowserCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext hasSelectedItems
]

{ #category : 'accessing' }
ClySpawnFullBrowserCommand >> description [

	^ 'Spawn full browser from current browser state.'
]

{ #category : 'execution' }
ClySpawnFullBrowserCommand >> executeRefactoring [

	self browser spawnFullBrowser
]

{ #category : 'accessing' }
ClySpawnFullBrowserCommand >> iconName [
	^#browse
]

{ #category : 'accessing' }
ClySpawnFullBrowserCommand >> name [
	^'Browse'
]

{ #category : 'accessing' }
ClySpawnFullBrowserCommand >> order [

	^ 1000
]
