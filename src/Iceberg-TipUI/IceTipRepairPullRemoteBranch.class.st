"
I will perform a pull from a remote, to bring put repository in sync.

matches if: 
	- workingCopy is detached AND there is no commit in the current available commits
"
Class {
	#name : #IceTipRepairPullRemoteBranch,
	#superclass : #IceTipRepairAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairPullRemoteBranch class >> help [
	^ 'You can pull commits from a remote to put your image and your repository in sync.
This will not lose any change in the image.

This situation happens because the code loaded in your image is forward the code present in the repository.

This operation will modify the state of your working copy in disk. All non-commited changes in your disk working copy that are not in the image will be lost. If you want to keep them, perform a commit from outside before.'
]

{ #category : #accessing }
IceTipRepairPullRemoteBranch class >> icon [
	^ self iconNamed: #down
]

{ #category : #testing }
IceTipRepairPullRemoteBranch class >> isAvailableFor: aRepository [
	"matches if: 
		- workingCopy is detached AND there is no commit in the current available commits"

	aRepository isMissing ifTrue: [ ^ false ].
	aRepository workingCopy referenceCommit isUnknownCommit ifTrue: [ ^ false ].
	aRepository workingCopy referenceCommit isNoCommit ifTrue: [ ^ false ].

	^ aRepository workingCopy isDetached and: [ (aRepository includesCommit: aRepository workingCopy referenceCommit) not ]
]

{ #category : #accessing }
IceTipRepairPullRemoteBranch class >> order [
	^ 17
]

{ #category : #accessing }
IceTipRepairPullRemoteBranch class >> title [
	^ 'Pull remote commits to put your repository in sync'
]

{ #category : #executing }
IceTipRepairPullRemoteBranch >> basicExecute [
	"Fetching remotes is necessary in case the current image commit is not in the repository.
	Could be optimized"
	self fetchAllRemotes.
	(IceTipPullBrowser onRepositoryModel: self repositoryModel) openWithSpec
]
