"
Command that discard all changes in the repository
"
Class {
	#name : #IceTipDiscardChangesCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipDiscardChangesCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryMissing not
		and: [ aToolContext repositoryModel entity workingCopy referenceCommit isCollection not
				and: [ aToolContext repositoryModel entity workingCopy referenceCommit isUnknownCommit not ] ]
]

{ #category : #accessing }
IceTipDiscardChangesCommand class >> defaultHelp [
	^ 'Discard all changes in this working copy.'
]

{ #category : #accessing }
IceTipDiscardChangesCommand class >> defaultMenuIconName [
	^ #refresh
]

{ #category : #accessing }
IceTipDiscardChangesCommand class >> defaultMenuItemName [
	^ 'Discard all'
]

{ #category : #accessing }
IceTipDiscardChangesCommand class >> defaultPosition [
	^ #right
]

{ #category : #accessing }
IceTipDiscardChangesCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipWorkingCopyContext
		order: 10
]

{ #category : #execution }
IceTipDiscardChangesCommand >> execute [
	self repositoryModel discardChanges
]
