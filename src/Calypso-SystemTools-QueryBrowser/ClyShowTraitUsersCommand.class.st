"
Retrieves the selected class traits.
"
Class {
	#name : 'ClyShowTraitUsersCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'activation' }
ClyShowTraitUsersCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyFullBrowserClassContext
]

{ #category : 'accessing' }
ClyShowTraitUsersCommand >> browser [

	^ browser
]

{ #category : 'accessing' }
ClyShowTraitUsersCommand >> browser: anObject [

	browser := anObject
]

{ #category : 'accessing' }
ClyShowTraitUsersCommand >> defaultMenuItemName [
	^'Show trait users'
]

{ #category : 'execution' }
ClyShowTraitUsersCommand >> execute [

	classes first isTrait ifTrue: [ (Smalltalk tools toolNamed: #messageList) browseUsersOfTrait: classes first ]
]

{ #category : 'execution' }
ClyShowTraitUsersCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	browser := aToolContext browser
]
