"
I am a command implementing the inspect option. I have several context activations to be able to inspect different objects in Iceberg's UI
"
Class {
	#name : #IceTipInspectCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipInspectCommand class >> branchListContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdExtraMenuGroup
		for: IceTipRepositoryContext
]

{ #category : #activation }
IceTipInspectCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdExtraMenuGroup
		for: IceTipWorkingCopyContext
]

{ #category : #activation }
IceTipInspectCommand class >> browserHistoryContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdExtraMenuGroup
		for: IceTipHistoryContext
]

{ #category : #activation }
IceTipInspectCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdExtraMenuGroup
		for: IceTipRepositoryListContext
]

{ #category : #activation }
IceTipInspectCommand class >> credentialsListContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdExtraMenuGroup
		for: IceTipCredentialContext
]

{ #category : #accessing }
IceTipInspectCommand class >> defaultHelp [
	^ 'Inspect the selected element'
]

{ #category : #accessing }
IceTipInspectCommand class >> defaultMenuIconName [
	^ #glamorousInspect
]

{ #category : #accessing }
IceTipInspectCommand class >> defaultMenuItemName [
	^ 'Inspect...'
]

{ #category : #execution }
IceTipInspectCommand >> execute [
	item ifNil: [ ^ self ].
	item entity inspect
]

{ #category : #execution }
IceTipInspectCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	item := aToolContext item
]
