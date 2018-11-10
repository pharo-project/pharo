"
I will perform the chechout of an existing branch, but I will remove all changes on image. 

I'm active when: 
	- working copy is detached 
	- repository HEAD is detached
"
Class {
	#name : #IceTipRepairCheckoutBranch,
	#superclass : #IceTipRepairAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairCheckoutBranch class >> help [
	^ 'Discard all changes in your image and checkout an existing branch.
This action will checkout the branch in the repository and align your image with your repository.

This situation happens because the code loaded in your image does not correspond with the status of your repository.

Opens a preview window before doing any change.

This operation will modify the state of your working copy in disk. All non-commited changes in your disk working copy that are not in the image will be lost. If you want to keep them, perform a commit from outside before.'
]

{ #category : #accessing }
IceTipRepairCheckoutBranch class >> icon [
	^ self iconNamed: #branch
]

{ #category : #testing }
IceTipRepairCheckoutBranch class >> isAvailableFor: aRepository [
	"matches if: 
		- workingCopy is detached
		- HEAD is detached"

	^ aRepository isMissing not
		and: [ aRepository project isUnborn or: [ aRepository workingCopy isDetached or: [ aRepository head isDetached ] ] ]
]

{ #category : #accessing }
IceTipRepairCheckoutBranch class >> order [
	^ 30
]

{ #category : #accessing }
IceTipRepairCheckoutBranch class >> title [
	^ 'Discard local changes and checkout an existing branch'
]

{ #category : #executing }
IceTipRepairCheckoutBranch >> basicExecute [
	self fetchAllRemotes.
	(IceTipCheckoutBranchDialog on: self repositoryModel) 
		withoutNewBranch;
		openDialogWithSpec
]
