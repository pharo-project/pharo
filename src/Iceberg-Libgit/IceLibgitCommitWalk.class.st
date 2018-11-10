"
Utility class for searching through a commit history taking advantage of git revwalk utilities. Found and returned commits will be added to the commit cache of the corresponding IceRepository.
"
Class {
	#name : #IceLibgitCommitWalk,
	#superclass : #IceAbstractCommitWalk,
	#instVars : [
		'revwalk',
		'lgitRepository'
	],
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #walking }
IceLibgitCommitWalk >> commitsDo: aBlock [ 
	self maxNumber 
		ifNil: [ super commitsDo: aBlock ]
		ifNotNil: [ | count |
			count := 0.
			super commitsDo: [ :commit |
				aBlock value: commit.
				count := count + 1.
				count == self maxNumber ifTrue: [ ^ self ]
			]
		]

]

{ #category : #'walk definition' }
IceLibgitCommitWalk >> fromBranch: branch [
	[ self revwalk pushReference: (lgitRepository lookupBranch: (branch isRemote ifTrue: [branch fullname] ifFalse: [ branch shortname ]) remote: branch isRemote) ]
		on: LGit_GIT_EINVALIDSPEC do: [
			"branch real branch, so try to find a corresponding treeish using revparse, 
			this will handle stuff like 'master~1'"
			self fromLGitId: (lgitRepository revparse: branch name) id
		]


]

{ #category : #'walk definition' }
IceLibgitCommitWalk >> fromCommit: commit [
	self fromCommitId: commit id
]

{ #category : #'walk definition' }
IceLibgitCommitWalk >> fromCommitId: commitId [
	self fromLGitId: (LGitId fromHexString: commitId)
]

{ #category : #'walk definition' }
IceLibgitCommitWalk >> fromHead [
	self revwalk pushHead
]

{ #category : #'walk definition' }
IceLibgitCommitWalk >> fromLGitId: lGitId [
	self revwalk pushCommit: lGitId
]

{ #category : #'walk definition' }
IceLibgitCommitWalk >> fromTag: tag [
	self revwalk.
	^ self fromLGitId: (lgitRepository revparse: tag name) id


]

{ #category : #private }
IceLibgitCommitWalk >> rawResultsDo: aBlockClosure [ 
	self revwalk 
		select: [ :result | self shouldInclude: result ]
		thenDo: aBlockClosure
]

{ #category : #accessing }
IceLibgitCommitWalk >> repositoryHandle [

	<repositoryHandleAccessor>	
	^ repository repositoryHandle
]

{ #category : #accessing }
IceLibgitCommitWalk >> revwalk [

	repository handleLibgitError: [
		^ revwalk
			ifNil: [ "Keep a reference to the repo so that it does not get garbage collected 
				(that would free C memory and make leave the revwalk object dumb)"
				lgitRepository := self repositoryHandle.
				revwalk := LGitRevwalk of: self repositoryHandle ]]
]

{ #category : #private }
IceLibgitCommitWalk >> shouldInclude: aLGitCommit [ 
	^ self modifyingPackage 
		ifNil: [ true ] 
		ifNotNil: [ :package | 
			aLGitCommit changesFileNamed: package directoryPathString ]

]

{ #category : #'walk definition' }
IceLibgitCommitWalk >> uptoCommit: commit [
	self revwalk hideCommit: (LGitId fromHexString: commit id)
]
