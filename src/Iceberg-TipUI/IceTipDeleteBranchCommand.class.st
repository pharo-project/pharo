"
Command that deletes the selected branch
"
Class {
	#name : #IceTipDeleteBranchCommand,
	#superclass : #IceTipRepositoryCommand,
	#instVars : [
		'branchModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipDeleteBranchCommand class >> actsOnBranch: aBranchModel [
	^ aBranchModel isLocal
]

{ #category : #execution }
IceTipDeleteBranchCommand class >> browserContextActivation [
    <classAnnotation>
    
    ^ CmdContextMenuActivation byRootGroupItemOrder: 100 for: IceTipRepositoryContext
]

{ #category : #execution }
IceTipDeleteBranchCommand class >> canBeExecutedInContext: aToolContext [
    ^ aToolContext item canActivateCommand: self
]

{ #category : #accessing }
IceTipDeleteBranchCommand class >> defaultHelp [
	^ 'Delete this branch for this repository'
]

{ #category : #accessing }
IceTipDeleteBranchCommand class >> defaultMenuIconName [

	^ #delete
]

{ #category : #accessing }
IceTipDeleteBranchCommand class >> defaultMenuItemName [
	^ 'Delete branch'
]

{ #category : #execution }
IceTipDeleteBranchCommand >> execute [
	branchModel delete
]

{ #category : #execution }
IceTipDeleteBranchCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	branchModel := aToolContext item.
]
