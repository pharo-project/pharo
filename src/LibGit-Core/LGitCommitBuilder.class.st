Class {
	#name : #LGitCommitBuilder,
	#superclass : #LGitRepositoryObject,
	#instVars : [
		'author',
		'committer',
		'tree',
		'parents',
		'message',
		'updateReference',
		'messageEncoding',
		'commitId'
	],
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #accessing }
LGitCommitBuilder >> author [
	^ author ifNil: [
		author := self repository defaultSignature ]
]

{ #category : #accessing }
LGitCommitBuilder >> author: anLGitSignature [
	author := anLGitSignature
]

{ #category : #accessing }
LGitCommitBuilder >> commitId [
	^ commitId ifNil: [ commitId := LGitId new ]
]

{ #category : #accessing }
LGitCommitBuilder >> commitId: anLGitId [
	commitId := anLGitId
]

{ #category : #'libgit-calls' }
LGitCommitBuilder >> commit_create: theCommitId repo: repo update_ref: update_ref author: theAuthor committer: theCommitter message_encoding: message_encoding message: theMessage tree: theTree parent_count: parent_count parents: parentsPointer [
	
	<todo:
		'try to use original function signature instead of passing ExternalAddress as last arg'>
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_commit_create #(LGitId * theCommitId , LGitRepository repo , String update_ref , LGitSignature * theAuthor , LGitSignature * theCommitter , String message_encoding , String theMessage , LGitTree theTree , int parent_count , void *parentsPointer))
		options: #(optCoerceNilToNull)
]

{ #category : #accessing }
LGitCommitBuilder >> committer [
	^ committer ifNil: [ 
		committer := self repository defaultSignature ]
]

{ #category : #accessing }
LGitCommitBuilder >> committer: anLGitSignature [
	committer := anLGitSignature
]

{ #category : #accessing }
LGitCommitBuilder >> message [
	^ message ifNil: [ message := 'empty message' ]
]

{ #category : #accessing }
LGitCommitBuilder >> message: aString [
	message := aString
]

{ #category : #accessing }
LGitCommitBuilder >> messageEncoding [
	^ messageEncoding ifNil: [ 'UTF-8' ]
]

{ #category : #accessing }
LGitCommitBuilder >> messageEncoding: aString [
	messageEncoding := aString
]

{ #category : #accessing }
LGitCommitBuilder >> parents [
	^ parents ifNil: [ parents := #() ]
]

{ #category : #accessing }
LGitCommitBuilder >> parents: aCollection [
	parents := aCollection
]

{ #category : #accessing }
LGitCommitBuilder >> tree [
	<todo: 'use proper exceptions'>
	^ tree ifNil: [ self error: 'need an existing tree here' ]
]

{ #category : #accessing }
LGitCommitBuilder >> tree: anLGitTree [
	tree := anLGitTree
]

{ #category : #accessing }
LGitCommitBuilder >> updateReference [
	^ updateReference
]

{ #category : #accessing }
LGitCommitBuilder >> updateReference: aReference [
	updateReference := aReference
]

{ #category : #operations }
LGitCommitBuilder >> writeCommit [
	| parentsPointer |
	self withReturnHandlerDo: [ 
		self assert: (self parents allSatisfy: #isReady).
		parentsPointer := self parents
			ifNotEmpty: [ (LGitExternalCommitArray fromCollection: self parents) getHandle ]
			ifEmpty: [ ExternalAddress null ].
	self withReturnHandlerDo: [ 
		self
			commit_create: self commitId
			"libgit2 asserts the identity of this repository and the one it retrieves for the tree, so they need to be the same here"
			repo: self tree owner 
			update_ref: self updateReference
			author: self author
			committer: self committer
			message_encoding: self messageEncoding
			message: self message
			tree: self tree
			parent_count: self parents size
			parents: parentsPointer] ]
]

{ #category : #accessing }
LGitCommitBuilder >> writtenCommit [
	<todo: 'use proper exceptions'>
	commitId ifNil: [ self error: 'haven''t written the commit yet!' ].
	^ LGitCommit of: self repository fromId: self commitId
]
