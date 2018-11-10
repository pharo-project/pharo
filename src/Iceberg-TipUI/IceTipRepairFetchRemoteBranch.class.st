"
I will fetch repositories to try to find the unknown commit. 

matches if: 
	- workingCopy referenceCommit isUnknownCommit

"
Class {
	#name : #IceTipRepairFetchRemoteBranch,
	#superclass : #IceTipRepairAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepairFetchRemoteBranch class >> help [
	^ 'You can fetch commits from a remote to discover your image commit in your repository.
This will not lose any change in the image.

This situation happens because the code loaded in your image is not in the branch currently selected on your repository.'
]

{ #category : #accessing }
IceTipRepairFetchRemoteBranch class >> icon [
	^ self iconNamed: #smallUpdate
]

{ #category : #testing }
IceTipRepairFetchRemoteBranch class >> isAvailableFor: aRepository [
	"matches if: 
		- workingCopy referenceCommit isUnknownCommit"
		
	^ aRepository isMissing not 
		and: [ aRepository workingCopy referenceCommit isUnknownCommit ]
]

{ #category : #accessing }
IceTipRepairFetchRemoteBranch class >> order [
	^ 15
]

{ #category : #accessing }
IceTipRepairFetchRemoteBranch class >> title [
	^ 'Fetch remote commits to find the reference commit'
]

{ #category : #executing }
IceTipRepairFetchRemoteBranch >> basicExecute [
	"Fetching remotes is necessary in case the current image commit is not in the repository.
	Could be optimized"
	self fetchAllRemotes.
]

{ #category : #executing }
IceTipRepairFetchRemoteBranch >> finishSuccess [
	super finishSuccess.
	Iceberg announcer announce: (IceRepositoryModified for: self repository)
]
