"
I am a command to show all methods in the groups in new method browser.

To show methods I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : 'ClyShowGroupsInMethodBrowserCommand',
	#superclass : 'ClyMethodGroupCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'activation' }
ClyShowGroupsInMethodBrowserCommand class >> fullBrowserDoubleClickMenuActivation [
	<classAnnotation>

	^(CmdDoubleClickActivation for: ClyFullBrowserMethodGroupContext)
		priority: 10000
]

{ #category : 'activation' }
ClyShowGroupsInMethodBrowserCommand class >> fullBrowserMethodGroupMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyFullBrowserMethodGroupContext
]

{ #category : 'accessing' }
ClyShowGroupsInMethodBrowserCommand >> defaultMenuIconName [
	^#browse
]

{ #category : 'accessing' }
ClyShowGroupsInMethodBrowserCommand >> defaultMenuItemName [
	^'Show in method browser'
]

{ #category : 'execution' }
ClyShowGroupsInMethodBrowserCommand >> execute [

	| queries fullQuery |
	queries := methodGroups collect: [:each | each methodQuery].
	fullQuery := ClyQuery unionFrom: queries.

	browser spawnQueryBrowserOn: fullQuery
]

{ #category : 'execution' }
ClyShowGroupsInMethodBrowserCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	browser := aToolContext browser
]
