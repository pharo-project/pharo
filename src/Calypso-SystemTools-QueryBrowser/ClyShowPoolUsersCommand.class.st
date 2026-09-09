"
Retrieves the selected class pool users.
"
Class {
	#name : 'ClyShowPoolUsersCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'activation' }
ClyShowPoolUsersCommand class >> fullBrowserMenuActivation [

	<classAnnotation>
	^ CmdContextMenuActivation
		  byItemOf: ClyQueryMenuGroup
		  for: ClyFullBrowserClassContext
]

{ #category : 'accessing' }
ClyShowPoolUsersCommand >> browser [

	^ browser
]

{ #category : 'accessing' }
ClyShowPoolUsersCommand >> browser: anObject [

	browser := anObject
]

{ #category : 'accessing' }
ClyShowPoolUsersCommand >> defaultMenuItemName [

	^ 'Show pool users'
]

{ #category : 'execution' }
ClyShowPoolUsersCommand >> execute [

	(Smalltalk tools toolNamed: #messageList) browseUsersOfSharedPool: classes first
]

{ #category : 'execution' }
ClyShowPoolUsersCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	browser := aToolContext browser
]
