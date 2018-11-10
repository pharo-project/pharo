"
Command that opens a window showing the history of the selected merged branch.
"
Class {
	#name : #IceTipMergeHistoryCommand,
	#superclass : #IceTipRepositoryCommand,
	#instVars : [
		'commitModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipMergeHistoryCommand class >> browserHistoryContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byRootGroupItemOrder: 100
		for: IceTipHistoryContext
]

{ #category : #testing }
IceTipMergeHistoryCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext selectedCommit hasMultipleAncestors
]

{ #category : #accessing }
IceTipMergeHistoryCommand class >> defaultHelp [
	^ 'Show the merged history of the selected merge commit'
]

{ #category : #activation }
IceTipMergeHistoryCommand class >> defaultMenuItemName [
	^ 'Browse merge history'
]

{ #category : #execution }
IceTipMergeHistoryCommand >> execute [
	(IceTipHistoryBrowser on: commitModel mergeHistoryFromSecondAncestor) openWithSpec
	
]

{ #category : #execution }
IceTipMergeHistoryCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	commitModel := aToolContext item
]
