Class {
	#name : #ClyShowTraitUsersCommand,
	#superclass : #SycClassCommand,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Queries'
}

{ #category : #activation }
ClyShowTraitUsersCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyFullBrowserClassContext
]

{ #category : #accessing }
ClyShowTraitUsersCommand >> browser [

	^ browser
]

{ #category : #accessing }
ClyShowTraitUsersCommand >> browser: anObject [

	browser := anObject
]

{ #category : #accessing }
ClyShowTraitUsersCommand >> defaultMenuItemName [
	^'Show trait users'
]

{ #category : #execution }
ClyShowTraitUsersCommand >> execute [

	classes first isTrait ifTrue: [ 
		browser spawnQueryBrowserOn: (ClyConstantQuery
				 returning: classes first traitUsers
				 as: (ClySortedQueryResult using: ClySortByNameFunction ascending)) ]
]

{ #category : #execution }
ClyShowTraitUsersCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	browser := aToolContext browser
]
