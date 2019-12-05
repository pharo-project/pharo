"
I am a node from a tree used to show results in DrTestUI.

"
Class {
	#name : #DTTreeNode,
	#superclass : #DTAbstractTreeNode,
	#instVars : [
		'name',
		'subResults',
		'contextMenuBlock',
		'subResultsAggregator',
		'browseBlock'
	],
	#category : #'DrTests-Model'
}

{ #category : #'instance creation' }
DTTreeNode class >> empty [
	^ self new
		subResults: #();
		yourself
]

{ #category : #visiting }
DTTreeNode >> acceptVisitor: aDTResultsTreeVisitor [
	^ aDTResultsTreeVisitor visitDTTreeNode: self
]

{ #category : #accessing }
DTTreeNode >> browseBlock [
	^ browseBlock
]

{ #category : #accessing }
DTTreeNode >> browseBlock: anObject [
	browseBlock := anObject
]

{ #category : #testing }
DTTreeNode >> canBeBrowsed [
	^ self browseBlock isNotNil
]

{ #category : #accessing }
DTTreeNode >> contentForReRun [
	^ (DTLeavesCollector collectLeavesOf: self) flatCollect: #contentForReRun
]

{ #category : #accessing }
DTTreeNode >> contextMenuBlock [
	^ contextMenuBlock
]

{ #category : #accessing }
DTTreeNode >> contextMenuBlock: anObject [
	contextMenuBlock := anObject
]

{ #category : #actions }
DTTreeNode >> drTestsBrowse [
	"Browse the tree node according to what is specified by my #browseBlock.
	 If my #browseBlock is nil, does nothing."
	self canBeBrowsed
		ifFalse: [ ^ self ].
	
	self browseBlock cull: self
]

{ #category : #menu }
DTTreeNode >> drTestsBuildContextMenu: menu [
	self contextMenuBlock value: menu
]

{ #category : #accessing }
DTTreeNode >> drTestsName [
	^ String streamContents: [ :s |
		s
			<< self name;
			<< $(;
			<< (self subResultsAggregator value: self subResults) asString;
			<< $) ]
]

{ #category : #initialization }
DTTreeNode >> initialize [
	super initialize.
	self
		contextMenuBlock: [ :menu | ]. "Does nothing by default."
	self subResultsAggregator: [ :subRes | (DTLeavesCollector collectLeavesOf: self) size ].
]

{ #category : #accessing }
DTTreeNode >> name [
	^ name
]

{ #category : #accessing }
DTTreeNode >> name: anObject [
	name := anObject
]

{ #category : #accessing }
DTTreeNode >> subResults [
	^ subResults
]

{ #category : #accessing }
DTTreeNode >> subResults: anObject [
	subResults := anObject
]

{ #category : #accessing }
DTTreeNode >> subResultsAggregator [
	^ subResultsAggregator
]

{ #category : #accessing }
DTTreeNode >> subResultsAggregator: anObject [
	subResultsAggregator := anObject
]
