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

{ #category : 'testing' }
ClyShowPoolUsersCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isClassSelected and: [ aToolContext lastSelectedClass isPool ]
]

{ #category : 'accessing' }
ClyShowPoolUsersCommand >> browser [

	^ browser
]

{ #category : 'accessing' }
ClyShowPoolUsersCommand >> browser: anObject [

	browser := anObject
]

{ #category : 'execution' }
ClyShowPoolUsersCommand >> executeRefactoring [

	(Smalltalk tools toolNamed: #messageList) browseUsersOfSharedPool: classes first
]

{ #category : 'testing' }
ClyShowPoolUsersCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext
]

{ #category : 'accessing' }
ClyShowPoolUsersCommand >> menuGroup [

	^ ClyQueryMenuGroup
]

{ #category : 'accessing' }
ClyShowPoolUsersCommand >> name [

	^ 'Show pool users'
]

{ #category : 'accessing' }
ClyShowPoolUsersCommand >> order [

	^ 1000
]

{ #category : 'execution' }
ClyShowPoolUsersCommand >> prepareFullExecution [

	super prepareFullExecution.
	browser := self context browser
]
