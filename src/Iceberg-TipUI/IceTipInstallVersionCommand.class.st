"
Command used in the version browser, inside the code browser.
This command takes the selected version and installs it (also called revert).
"
Class {
	#name : #IceTipInstallVersionCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'selectedCommit',
		'version'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipInstallVersionCommand class >> browserVersionHistoryContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byRootGroupItemOrder: 1
		for: IceTipVersionHistoryContext
]

{ #category : #accessing }
IceTipInstallVersionCommand class >> defaultHelp [
	^ 'Revert to the selected version'
]

{ #category : #activation }
IceTipInstallVersionCommand class >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : #activation }
IceTipInstallVersionCommand class >> defaultMenuItemName [
	^ 'Install this version'
]

{ #category : #execution }
IceTipInstallVersionCommand >> execute [
	
	version install
]

{ #category : #execution }
IceTipInstallVersionCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	version := aToolContext version.
	
]
