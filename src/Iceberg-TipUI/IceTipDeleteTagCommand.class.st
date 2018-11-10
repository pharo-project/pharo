"
Command that deletes the selected tag
"
Class {
	#name : #IceTipDeleteTagCommand,
	#superclass : #IceTipRepositoryCommand,
	#instVars : [
		'tagModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipDeleteTagCommand class >> actsOnTag: aRemoteModel [
	^ true
]

{ #category : #execution }
IceTipDeleteTagCommand class >> browserContextActivation [
    <classAnnotation>
    
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 100
		for: IceTipRepositoryContext
]

{ #category : #execution }
IceTipDeleteTagCommand class >> canBeExecutedInContext: aToolContext [
    ^ aToolContext item canActivateCommand: self
]

{ #category : #accessing }
IceTipDeleteTagCommand class >> defaultHelp [
	^ 'Delete this tag for this repository'
]

{ #category : #accessing }
IceTipDeleteTagCommand class >> defaultMenuIconName [
	^ #delete
]

{ #category : #accessing }
IceTipDeleteTagCommand class >> defaultMenuItemName [
	^ 'Delete tag'
]

{ #category : #execution }
IceTipDeleteTagCommand >> execute [
	tagModel delete
]

{ #category : #execution }
IceTipDeleteTagCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	tagModel := aToolContext item.
]
