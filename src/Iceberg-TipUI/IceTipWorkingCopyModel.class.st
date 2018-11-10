"
I'm a model to for IceWorkingCopy entries.
"
Class {
	#name : #IceTipWorkingCopyModel,
	#superclass : #IceTipModel,
	#instVars : [
		'sortingStrategy',
		'repositoryModel'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #'instance creation' }
IceTipWorkingCopyModel class >> new [
	self error: 'use #for:'
]

{ #category : #'instance creation' }
IceTipWorkingCopyModel class >> on: aRepositoryModel [ 
	^ self basicNew 
		initializeRepositoryModel: aRepositoryModel;
		yourself
]

{ #category : #accessing }
IceTipWorkingCopyModel >> branchName [
	^ self repositoryModel branchName
]

{ #category : #accessing }
IceTipWorkingCopyModel >> commitId [
	^ repositoryModel commitId
]

{ #category : #operations }
IceTipWorkingCopyModel >> copyCommitIDToClipboard [
	<noCache>

	repositoryModel copyCommitIDToClipboard
]

{ #category : #initialization }
IceTipWorkingCopyModel >> initializeRepositoryModel: aRepositoryModel [
	repositoryModel := aRepositoryModel.
	self initialize
]

{ #category : #accessing }
IceTipWorkingCopyModel >> packageModels [
	^ self sortingStrategy sort: (self repositoryModel entity workingCopy packages 
		collect: [ :each | 
			(IceTipPackageModel 
				repositoryModel: self repositoryModel 
				on: each) 
				beCached ])
]

{ #category : #accessing }
IceTipWorkingCopyModel >> repositoryModel [
	^ repositoryModel
]

{ #category : #accessing }
IceTipWorkingCopyModel >> repositoryName [
	^ self repositoryModel name
]

{ #category : #accessing }
IceTipWorkingCopyModel >> reset [
	self repositoryModel reset
]

{ #category : #accessing }
IceTipWorkingCopyModel >> shortCommitId [
	^ repositoryModel shortCommitId
]

{ #category : #accessing }
IceTipWorkingCopyModel >> sortingStrategy [
	^ sortingStrategy ifNil: [ sortingStrategy := IceSortingStrategy new first: #modified ]
]

{ #category : #accessing }
IceTipWorkingCopyModel >> sortingStrategyFirst: aSymbol [
	"they can be #modified, #loaded, #none" 
	self sortingStrategy first: aSymbol
]

{ #category : #accessing }
IceTipWorkingCopyModel >> status [
	^ self repositoryModel status
]
