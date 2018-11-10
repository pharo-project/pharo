"
Command that opens a dialog to edit a repository meta-data
"
Class {
	#name : #IceTipEditRepositoryCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipEditRepositoryCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 8000
		for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipEditRepositoryCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryOperational
]

{ #category : #accessing }
IceTipEditRepositoryCommand class >> defaultHelp [
	^ 'Edit repository data'
]

{ #category : #accessing }
IceTipEditRepositoryCommand class >> defaultMenuIconName [
	^ #edit
]

{ #category : #accessing }
IceTipEditRepositoryCommand class >> defaultMenuItemName [
	^ 'Edit...'
]

{ #category : #execution }
IceTipEditRepositoryCommand >> execute [
	self repositoryModel edit
]
