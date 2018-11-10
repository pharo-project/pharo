"
I represent the state of a working copy. I have several methods to combine myself with package states and generate new working copy states.
"
Class {
	#name : #IceWorkingCopyState,
	#superclass : #Object,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-WorkingCopy'
}

{ #category : #'as yet unclassified' }
IceWorkingCopyState class >> repository: aRepository [ 

	^ self new
		repository: aRepository;
		yourself
]

{ #category : #display }
IceWorkingCopyState >> description [

	self subclassResponsibility
]

{ #category : #testing }
IceWorkingCopyState >> isCommitMerged: aCommit [

	self subclassResponsibility
]

{ #category : #testing }
IceWorkingCopyState >> isDetached [
	
	^ self subclassResponsibility
]

{ #category : #testing }
IceWorkingCopyState >> isInMerge [
	
	^ false
]

{ #category : #testing }
IceWorkingCopyState >> isSingleCommitState [
	
	^ false
]

{ #category : #testing }
IceWorkingCopyState >> isUnknownCommitState [
	
	^ false
]

{ #category : #merging }
IceWorkingCopyState >> mergeCommit: aCommit [
	
	self subclassResponsibility
]

{ #category : #merging }
IceWorkingCopyState >> mergeStateWith: aMergeableState [
	
	self subclassResponsibility
]

{ #category : #accessing }
IceWorkingCopyState >> referenceCommit [
	
	^ repository workingCopy referenceCommit
]

{ #category : #accessing }
IceWorkingCopyState >> referenceCommits [
	
	^ { self referenceCommit }
]

{ #category : #accessing }
IceWorkingCopyState >> repository [
	^ repository
]

{ #category : #accessing }
IceWorkingCopyState >> repository: anIceLibgitRepository [ 
	repository := anIceLibgitRepository
]

{ #category : #validating }
IceWorkingCopyState >> validateCanCommit [
	
	self subclassResponsibility
]

{ #category : #accessing }
IceWorkingCopyState >> workingCopy [
	
	^ repository workingCopy
]
