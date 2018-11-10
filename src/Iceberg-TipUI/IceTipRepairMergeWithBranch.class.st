"
I will perform a switch and merge of current reference commit with the selected branch. 

I'm active when: 
	- working copy is detached 
	- repository HEAD is NOT detached
"
Class {
	#name : #IceTipRepairMergeWithBranch,
	#superclass : #IceTipRepairAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairMergeWithBranch class >> help [
	^ 'You can checkout a new or existing branch and merge your image into it.
This will not lose any change in the image but open a merge view is necessary.

This situation happens because the code loaded in your image does not correspond with the status of your repository.

This operation will modify the state of your working copy in disk. All non-commited changes in your disk working copy that are not in the image will be lost. If you want to keep them, perform a commit from outside before.'
]

{ #category : #accessing }
IceTipRepairMergeWithBranch class >> icon [
	^ self iconNamed: #merge
]

{ #category : #testing }
IceTipRepairMergeWithBranch class >> isAvailableFor: aRepository [
	"matches if: 
		- workingCopy is detached
		- HEAD is NOT detached"
		
	aRepository isMissing ifTrue: [ ^ false ].
	aRepository workingCopy referenceCommit isUnknownCommit ifTrue: [ ^ false ].
	aRepository workingCopy referenceCommit isNoCommit ifTrue: [ ^ false ].
		
	^ aRepository workingCopy isDetached and: [ aRepository head isDetached not ]
]

{ #category : #accessing }
IceTipRepairMergeWithBranch class >> order [
	^ 20
]

{ #category : #accessing }
IceTipRepairMergeWithBranch class >> title [
	^ 'Merge your image with an existing branch'
]

{ #category : #executing }
IceTipRepairMergeWithBranch >> basicExecute [
	self fetchAllRemotes.
	(IceTipMergeBranchDialog on: self repositoryModel) 
		beSwitchAndMerge;
		openDialogWithSpec
]
