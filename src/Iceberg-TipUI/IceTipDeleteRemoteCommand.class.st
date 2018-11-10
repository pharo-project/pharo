"
Command that deletes the selected remote
"
Class {
	#name : #IceTipDeleteRemoteCommand,
	#superclass : #IceTipRepositoryCommand,
	#instVars : [
		'branchModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipDeleteRemoteCommand class >> actsOnRemote: aRemoteModel [
	^ true
]

{ #category : #execution }
IceTipDeleteRemoteCommand class >> browserContextActivation [
    <classAnnotation>
    
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 100
		for: IceTipRepositoryContext
]

{ #category : #execution }
IceTipDeleteRemoteCommand class >> canBeExecutedInContext: aToolContext [
    ^ aToolContext item canActivateCommand: self
]

{ #category : #accessing }
IceTipDeleteRemoteCommand class >> defaultHelp [
	^ 'Delete this remote for this repository'
]

{ #category : #accessing }
IceTipDeleteRemoteCommand class >> defaultMenuIconName [
	^ #delete
]

{ #category : #accessing }
IceTipDeleteRemoteCommand class >> defaultMenuItemName [
	^ 'Delete remote'
]

{ #category : #execution }
IceTipDeleteRemoteCommand >> execute [
	branchModel delete
]

{ #category : #execution }
IceTipDeleteRemoteCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	branchModel := aToolContext item.
]
