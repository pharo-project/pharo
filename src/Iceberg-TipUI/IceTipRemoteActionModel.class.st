Class {
	#name : #IceTipRemoteActionModel,
	#superclass : #IceTipCommitishModel,
	#instVars : [
		'remote'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipRemoteActionModel >> branchName [
	^ entity branchName
]

{ #category : #testing }
IceTipRemoteActionModel >> isRepositoryDetached [
	^ self entity workingCopy isDetached 
		or: [ self entity head isDetached ]
]

{ #category : #accessing }
IceTipRemoteActionModel >> remote [
	^ remote ifNil: [ | branch |
		branch := self repositoryModel entity branch.
		remote := branch hasUpstream
				ifTrue: [ branch upstream remote ]
				ifFalse: [ self remoteModels first entity ] ]
]

{ #category : #accessing }
IceTipRemoteActionModel >> remoteModel [
	^ repositoryModel remoteModels detect: [ :e | e entity = self remote ]
]

{ #category : #accessing }
IceTipRemoteActionModel >> remoteModels [
	^ self repositoryModel remoteModels
]

{ #category : #accessing }
IceTipRemoteActionModel >> remoteName [
	^ self remote name, '/', self entity branch name
]

{ #category : #accessing }
IceTipRemoteActionModel >> repositoryName [
	^ self repositoryModel name
]

{ #category : #accessing }
IceTipRemoteActionModel >> useRemote: aRemoteModel [
	remote := aRemoteModel entity.
]
