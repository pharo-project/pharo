"
I am a working copy whose packages are all in merge status in the same commits.
"
Class {
	#name : #IceInMergeWorkingCopy,
	#superclass : #IceWorkingCopyState,
	#category : #'Iceberg-WorkingCopy'
}

{ #category : #display }
IceInMergeWorkingCopy >> description [
	
	^ ', ' join: (self referenceCommits collect: #description)
]

{ #category : #testing }
IceInMergeWorkingCopy >> isDetached [

	^ self referenceCommits first ~= self repository headCommit
]

{ #category : #testing }
IceInMergeWorkingCopy >> isInMerge [
	
	^ true
]

{ #category : #merging }
IceInMergeWorkingCopy >> mergeStateWith: aMergeableState [
	
	^ aMergeableState mergeWithInMergeWorkingCopyState: self
]

{ #category : #accessing }
IceInMergeWorkingCopy >> referenceCommits [
	
	^ self workingCopy referenceCommit
]

{ #category : #validating }
IceInMergeWorkingCopy >> validateCanCommit [
	
	self workingCopy referenceCommit first = repository head commit
		ifFalse: [ IceWorkingCopyDesyncronized signal ]
]
