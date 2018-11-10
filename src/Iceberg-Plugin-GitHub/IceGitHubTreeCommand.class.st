"
I represent all the Github commands that work on a Tree item
"
Class {
	#name : #IceGitHubTreeCommand,
	#superclass : #IceGitHubCommand,
	#instVars : [
		'remote'
	],
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #testing }
IceGitHubTreeCommand class >> actsOnBranch: anIceTipTreeBranch [ 
	^ false.
]

{ #category : #testing }
IceGitHubTreeCommand class >> actsOnRemote: aRemoteModel [
	^ true
]

{ #category : #testing }
IceGitHubTreeCommand class >> actsOnTag: aRemoteModel [
	^ false
]

{ #category : #activation }
IceGitHubTreeCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: IceGitHubMenuGroup
		for: IceTipRepositoryContext
]

{ #category : #testing }
IceGitHubTreeCommand class >> canBeExecutedInContext: aToolContext [
	^ (super canBeExecutedInContext: aToolContext) and: [ aToolContext item canActivateCommand: self ]
]

{ #category : #'context menu' }
IceGitHubTreeCommand >> fillContextMenu: aMenu using: aCommandActivator [
	super fillContextMenu: aMenu using: aCommandActivator.
	remote := aCommandActivator context item model entity
]

{ #category : #accessing }
IceGitHubTreeCommand >> remote [
	^ remote
]
