"
Base class for shared code between different commit walk implementations.
"
Class {
	#name : #IceAbstractCommitWalk,
	#superclass : #Object,
	#instVars : [
		'repository',
		'modifyingPackage',
		'maxNumber'
	],
	#category : #'Iceberg-Adapters'
}

{ #category : #'instance creation' }
IceAbstractCommitWalk class >> forRepository: repository [
	^ self new repository: repository; yourself
]

{ #category : #private }
IceAbstractCommitWalk >> commitFrom: result [
	^ self repository commitFromGitCommit: result
]

{ #category : #walking }
IceAbstractCommitWalk >> commits [
	| commits |
	commits := OrderedCollection new.
	self commitsDo: [ :commit | commits add: commit ].
	^ commits
]

{ #category : #walking }
IceAbstractCommitWalk >> commitsDo: aBlock [ 
	self rawResultsDo: [ :result | aBlock value: (self commitFrom: result) ]
]

{ #category : #walking }
IceAbstractCommitWalk >> firstCommit [
	^ self commitsDo: [ :commit | ^ commit ] 
]

{ #category : #'instance creation' }
IceAbstractCommitWalk >> fromCommit: aCommit [

	self subclassResponsibility
]

{ #category : #'instance creation' }
IceAbstractCommitWalk >> fromTag: anIceTag [ 
	
	self subclassResponsibility
]

{ #category : #walking }
IceAbstractCommitWalk >> includesCommit: aCommit [
	self commitsDo: [ :commit | 
		commit id = aCommit id ifTrue: [ ^ true ]].
	^ false
]

{ #category : #accessing }
IceAbstractCommitWalk >> maxNumber [
	^ maxNumber
]

{ #category : #accessing }
IceAbstractCommitWalk >> maxNumber: anObject [
	maxNumber := anObject
]

{ #category : #accessing }
IceAbstractCommitWalk >> modifyingPackage [
	^ modifyingPackage
]

{ #category : #accessing }
IceAbstractCommitWalk >> modifyingPackage: anObject [
	modifyingPackage := anObject
]

{ #category : #accessing }
IceAbstractCommitWalk >> repository [
	^ repository
]

{ #category : #accessing }
IceAbstractCommitWalk >> repository: anObject [
	repository := anObject
]

{ #category : #'walk definition' }
IceAbstractCommitWalk >> upto: aCommitish [
	self uptoCommit: aCommitish commit
]

{ #category : #'walk definition' }
IceAbstractCommitWalk >> uptoBranch: anIceBranch [ 
	self uptoCommit: anIceBranch lastCommit
]

{ #category : #'walk definition' }
IceAbstractCommitWalk >> uptoBranches: branches [
	^ branches do: [ :branch | self uptoBranch: branch ]
]

{ #category : #'walk definition' }
IceAbstractCommitWalk >> uptoCommits: commits [
	commits do: [ :commit | self uptoCommit: commit  ]
]

{ #category : #'walk definition' }
IceAbstractCommitWalk >> uptoVersion: version [
	version hideYourselfFromCommitWalk: self

]

{ #category : #walking }
IceAbstractCommitWalk >> versionsFor: package [
	self modifyingPackage: package.
	^ self commits collect: [ :commit | commit versionFor: package ]

]

{ #category : #walking }
IceAbstractCommitWalk >> versionsFor: package detect: selectBlock ifNone: ifNoneBlock [
	self modifyingPackage: package.
	self commitsDo: [ :commit | 
		| version | 
		version := commit versionFor: package.
		(selectBlock value: version) ifTrue: [ ^ version ]
	].

	^ ifNoneBlock value
  
]
