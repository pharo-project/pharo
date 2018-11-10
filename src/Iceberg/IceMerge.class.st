"
I implement the merge between two versions. I calculate the diff tree of each version and then calculate a merge tree containing the operations that should be applied to the right target tree to arrive to the left tree.

I am created by doing one of the following:

- IceMerge from: aVersion to: anotherVersion.

- IceMerge new
	sourceVersion: aVersion;
	targetVersion: anotherVersion;
	build;
	yourself
"
Class {
	#name : #IceMerge,
	#superclass : #Object,
	#instVars : [
		'mergeTree',
		'repository',
		'mergeCommit',
		'imageCommit',
		'changesToWorkingCopyTree'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #executing }
IceMerge >> calculateChanges [

	| commonAncestor sourceDiff targetDiff |

	mergeTree ifNotNil: [ ^ self ].

	commonAncestor := (self leftCommit commonAncestorWith: mergeCommit) ifNil: [ IceNoCommit new ].
	sourceDiff := self sourceVersion diffTo: commonAncestor.
	targetDiff := self targetVersion diffTo: commonAncestor.

	mergeTree := self merge: sourceDiff tree
		into: targetDiff tree.
	
	changesToWorkingCopyTree := self isInverseMerge 
		ifTrue: [  self merge: targetDiff tree into: sourceDiff tree ] 
		ifFalse: [ mergeTree  ]
	 
]

{ #category : #accessing }
IceMerge >> changesToWorkingCopyTree [

	self calculateChanges.
	^ changesToWorkingCopyTree
]

{ #category : #enumerating }
IceMerge >> conflictsDo: aBlockClosure [ 
	
	mergeTree do: [ :each |
		each isConflict ifTrue: [ aBlockClosure value: each value ] ]
]

{ #category : #executing }
IceMerge >> execute [
	"at begining of this, there is a possibility that imageCommit needs to be 'fixed' to use."
	| commitToAdvance |
	mergeCommit validateCanMerge.
	self isAlreadyMerged ifTrue: [ ^ self ].

	" 1 - Merge changes.
	If the image and merge commit are the same, and the headCommit is their ancestor, we should do nothing.
	Otherwise, we should merge the two commits we have, discerning well which is the left and which is the right commit." 
	self resolveConflicts.
	commitToAdvance := self updateHead.
	
	self updateWorkingCopy.
	
	^ commitToAdvance
]

{ #category : #testing }
IceMerge >> hasConflicts [
	
	self calculateChanges.
	^ mergeTree anySatisfy: [ :e | e isConflict ]
]

{ #category : #accessing }
IceMerge >> headCommit [
	^ repository headCommit
]

{ #category : #accessing }
IceMerge >> imageCommit [
	
	^ imageCommit ifNil: [ imageCommit := repository workingCopy referenceCommit ]
]

{ #category : #testing }
IceMerge >> isAlreadyMerged [
	"The commit is already merged in the image and in the head commit.
	This means the selected commit is already in the history and there is nothing to do about it."

	^ mergeCommit isAncestorOf: self leftCommit
]

{ #category : #testing }
IceMerge >> isEmpty [
	
	^ mergeTree isEmpty
]

{ #category : #testing }
IceMerge >> isFastForward [
	^ self headCommit isAncestorOf: mergeCommit
]

{ #category : #testing }
IceMerge >> isInverseMerge [
	"When the image commit corresponds to the mergeCommit. 
	It means we are in an inverse merge. We want to merge the changes from the image into head commit.
	
	This is false when the image commit corresponds with head. In this other scenario, we want to merge the changes from the merge commit into head commit."
	^ self imageCommit = mergeCommit
]

{ #category : #testing }
IceMerge >> isMerged [
	
	^ mergeTree allSatisfy: [ :operation |
		operation isConflict not or: [ operation isResolved ] ]
]

{ #category : #accessing }
IceMerge >> leftCommit [

	^ self isInverseMerge
		ifTrue: [ self headCommit ]
		ifFalse: [ self imageCommit ].

]

{ #category : #building }
IceMerge >> merge: changeTree into: currentTree [

	^ changeTree collectWithPath: [ :operation :path | 
			currentTree 
				resolve: path 
				ifPresent: [ :currentOperation | operation mergeWithOperation: currentOperation ] 
				ifAbsent: [ IceNonConflictingOperation operation: operation] ]
]

{ #category : #accessing }
IceMerge >> mergeCommit: aCommit [ 
	mergeCommit := aCommit
]

{ #category : #accessing }
IceMerge >> mergeTree [
	self calculateChanges.
	^ mergeTree
]

{ #category : #accessing }
IceMerge >> repository [
	
	^ repository
]

{ #category : #accessing }
IceMerge >> repository: anIceRepository [ 
	repository := anIceRepository
]

{ #category : #executing }
IceMerge >> resolveConflicts [
	self hasConflicts
		ifTrue: [ | resolved |
			"Abort merge and signal for manual merge. 
				Index is not saved and repository will be discarded, 
				so no further action is needed to abort the merge"
			resolved := IceMergeResolutionRequest new
				merger: self;
				signal: 'Merging ' , mergeCommit description , ' into ' , self leftCommit description.
			resolved
				ifFalse: [ IceMergeAborted signal ] ]
]

{ #category : #descriptions }
IceMerge >> sourceDescription [
	^ 'Working Copy version: ' , self repository workingCopy description
]

{ #category : #accessing }
IceMerge >> sourceVersion [
	"This is the commit that will provide the changes. 
	In a merge, the commit providing the changes is the merge commit."

	^ mergeCommit
]

{ #category : #descriptions }
IceMerge >> targetDescription [
	^ 'Merging commit: ' , self sourceVersion description
]

{ #category : #accessing }
IceMerge >> targetVersion [
	"This is the commit that will receive the changes. 
	In a merge, the commit receiving the changes is the head of the repository."
	^ self leftCommit
]

{ #category : #accessing }
IceMerge >> tree [
	"Returns the tree to merge.
	Polymorphic with an ice Diff"
	^ self mergeTree
]

{ #category : #accessing }
IceMerge >> treeToShow [
	"Returns the tree to show in the UI.
	Polymorphic with an ice Diff"
	^ self changesToWorkingCopyTree
]

{ #category : #accessing }
IceMerge >> unresolvedConflictCount [

	^ mergeTree count: [ :operation |
		operation isConflict and: [ operation isResolved not ] ]
]

{ #category : #executing }
IceMerge >> updateHead [
	" 2 - Update HEAD.
	If the merged commits are from diverging branches, generate a merge commit.
	Then, adopt the commit in
	 - the working copy
	 - the current branch"

	| commitToAdvance |
	commitToAdvance := self isFastForward
		ifTrue: [ "Fast-forward. There is no need to update the working copy in disk.
			Changing the commit in the branch below will checkout the commit in disk."
			repository workingCopy adoptCommit: mergeCommit.
			mergeCommit ]
		ifFalse: [ "Merge case"
			repository workingCopy
				setMergeStateBetweenCommits:
					{self leftCommit.
					mergeCommit}.
			repository workingCopy commitChanges: self withMessage: 'Merge ' , mergeCommit id asString force: true ].
	repository branch commit: commitToAdvance
]

{ #category : #executing }
IceMerge >> updateWorkingCopy [

	" 3 - Load changes in the image.
	In case the commit was already in the history, merge is a null object with no changes"
	repository workingCopy loadChangesInWorkingCopy: self changesToWorkingCopyTree.
	repository workingCopy refreshDirtyPackages
]
