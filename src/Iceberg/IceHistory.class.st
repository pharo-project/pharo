"
I provide optimized access to the history of a commitish. I access the history of a commit in a batch, caching them in an internal collection to provide fast access.

Moreover, I implement fast access to history information such as knowing if a commit is merged in head or not.

Future optimizations: 
 - implement pagination
"
Class {
	#name : #IceHistory,
	#superclass : #Object,
	#instVars : [
		'commitish',
		'commits',
		'commitDictionary',
		'headHistory',
		'repository'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceHistory class >> on: commitish repository: anIceRepository [
	
	^ self new
		commitish: commitish;
		repository: anIceRepository;
		yourself
]

{ #category : #history }
IceHistory >> commitDictionary [

	commitDictionary ifNil: [ self lookupCommits ].
	^ commitDictionary
]

{ #category : #accessing }
IceHistory >> commitish: aCommitish [
	commitish := aCommitish
]

{ #category : #history }
IceHistory >> commits [
	
	commits ifNil: [ self lookupCommits ].
	^ commits
]

{ #category : #accessing }
IceHistory >> headHistory [
	
	^ headHistory ifNil: [ 
		headHistory := IceHistory on: repository headCommit repository: repository ]
]

{ #category : #testing }
IceHistory >> includes: anIceGitCommit [ 
	
	^ self commitDictionary includesKey: anIceGitCommit id
]

{ #category : #testing }
IceHistory >> isHead [
	
	^ commitish = repository head
]

{ #category : #history }
IceHistory >> lookupCommits [
	
	| newCommitDictionary newCommits |
	newCommitDictionary := Dictionary new.
	newCommits := OrderedCollection new.
	commitish commitsDo: [ :each | | commit |
		commit := IceHistoryCommit commit: each history: self.
		newCommits add: commit.
		newCommitDictionary at: each id put: commit ].
	commitDictionary := newCommitDictionary.
	commits := newCommits.
]

{ #category : #accessing }
IceHistory >> name [
	
	^ commitish name
]

{ #category : #accessing }
IceHistory >> repository: aRepository [ 
	repository := aRepository
]
