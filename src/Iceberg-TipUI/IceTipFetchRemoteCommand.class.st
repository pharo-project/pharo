"
Command that fetches from remote repositories
"
Class {
	#name : #IceTipFetchRemoteCommand,
	#superclass : #IceTipRepositoryCommand,
	#instVars : [
		'remoteModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipFetchRemoteCommand class >> actsOnRemote: aRemoteModel [
	^ true
]

{ #category : #activation }
IceTipFetchRemoteCommand class >> browserContextActivation [
 	<classAnnotation>
    
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 10
		for: IceTipRepositoryContext
]

{ #category : #accessing }
IceTipFetchRemoteCommand class >> canBeExecutedInContext: aToolContext [
    ^ aToolContext item canActivateCommand: self
]

{ #category : #accessing }
IceTipFetchRemoteCommand class >> defaultHelp [
	^ 'Fetch commits from this remote'
]

{ #category : #accessing }
IceTipFetchRemoteCommand class >> defaultMenuIconName [
	^ #smallUpdate
]

{ #category : #accessing }
IceTipFetchRemoteCommand class >> defaultMenuItemName [
	^ 'Fetch remote'
]

{ #category : #execution }
IceTipFetchRemoteCommand >> execute [
	remoteModel fetch
]

{ #category : #execution }
IceTipFetchRemoteCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	remoteModel := aToolContext item.
]
