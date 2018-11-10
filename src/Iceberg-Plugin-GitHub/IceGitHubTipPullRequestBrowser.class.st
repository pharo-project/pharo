"
Description
--------------------

I a browser centered on a Pull Request. 

I display the list of commits of the PR for review, allow the user to merge the PR and to accept/reject it of the user has the rights on the repository.

"
Class {
	#name : #IceGitHubTipPullRequestBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'repositoryModel',
		'pullRequest',
		'commitLabel',
		'commits',
		'diff',
		'mergeButton',
		'acceptButton',
		'rejectButton'
	],
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #specs }
IceGitHubTipPullRequestBrowser class >> defaultSpec [
	^ SpecLayout composed
		newColumn: [ :mainC | 
			mainC
				newRow: [ :row | 
					row
						newColumn: [ :column | 
							column
								newRow: #commitLabel height: self labelHeight;
								newRow: #commits ] ];
				addHSplitter;
				newRow: [ :row | row add: #diff ];
				newRow: [ :row | 
					row
						add: #mergeButton;
						add: #acceptButton;
						add: #rejectButton ] height: self toolbarHeight ];
		yourself
]

{ #category : #'instance creation' }
IceGitHubTipPullRequestBrowser class >> on: aModel pullRequest: aPullRequest [
	^ self basicNew
		pullRequest: aPullRequest;
		setModelBeforeInitialization: aModel;
		initialize;
		yourself
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser class >> taskbarIcon [
	^ self iconNamed: #komitterSmalltalkhubRemote
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> acceptButton [
	^ acceptButton
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> acceptButton: anObject [
	acceptButton := anObject
]

{ #category : #actions }
IceGitHubTipPullRequestBrowser >> acceptPullRequest [
	IceGitHubAcceptPullRequestModel new
		onAccept: [ :model | 
			IceGitHubAPI ensureCredentials.
			self pullRequest acceptMessage: model message type: model type ];
		openWithSpec
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> commitLabel [
	^ commitLabel
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> commitLabel: anObject [
	commitLabel := anObject
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> commits [
	^ commits
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> commits: anObject [
	commits := anObject
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> diff [
	^ diff
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> diff: anObject [
	diff := anObject
]

{ #category : #api }
IceGitHubTipPullRequestBrowser >> initialExtent [
	^ (1050 @ 800) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceGitHubTipPullRequestBrowser >> initializeCommitsTable [
	| textStylingBlock |
	textStylingBlock := [ :string :commit | 
	commit isMerged
		ifTrue: [ string asText ]
		ifFalse: [ string asText allBold ] ].
	
	commits widget
		beResizable;
		addColumn: (IceTipTableColumn id: 'Date & time' action: [ :commit | textStylingBlock value: commit datetime asLocalStringYMDHM value: commit ] width: 120 * World displayScaleFactor);
		addColumn: (IceTipTableColumn id: 'Id' action: [ :commit | textStylingBlock value: commit shortId value: commit ] width: 50 * World displayScaleFactor);
		addColumn: (IceTipTableColumn id: 'Author' action: [ :commit | textStylingBlock value: commit author value: commit ] width: 120 * World displayScaleFactor);
		addColumn: (IceTipTableColumn id: 'Comment' action: [ :commit | textStylingBlock value: commit comment value: commit ] width: 300 * World displayScaleFactor);
		dataSource: self newCommitsDataSource;
		selectRowIndex: 1
]

{ #category : #initialization }
IceGitHubTipPullRequestBrowser >> initializeDiff [
	diff beForMerge.
	diff selectedModel: self commits widget dataSource elements anyOne
]

{ #category : #initialization }
IceGitHubTipPullRequestBrowser >> initializePresenter [
	super initializePresenter.
	self commits widget onAnnouncement: FTSelectionChanged send: #uptadeSelectedCommit: to: self.
	
	mergeButton action: [ self mergePullRequestIntoImage ].
	acceptButton action: [ self acceptPullRequest ].
	rejectButton action: [ self rejectPullRequest ]
]

{ #category : #initialization }
IceGitHubTipPullRequestBrowser >> initializeWidgets [
	commitLabel := self newLabel.
	commits := self newIceTable.
	diff := self instantiate: IceTipCommitInfoPresenter.
	mergeButton := self newButton.
	acceptButton := self newButton.
	rejectButton := self newButton.
	
	commitLabel label: 'Commits'.
	self initializeCommitsTable.
	self initializeDiff.
	mergeButton label: 'Merge into image'.
	acceptButton label: 'Accept pull request'.
	rejectButton label: 'Reject pull request'.
	
	self focusOrder
		add: commits;
		add: diff;
		add: mergeButton;
		add: acceptButton;
		add: rejectButton
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> mergeButton [
	^ mergeButton
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> mergeButton: anObject [
	mergeButton := anObject
]

{ #category : #actions }
IceGitHubTipPullRequestBrowser >> mergePullRequestIntoImage [
	(UIManager default confirm: ('I will create a new local branch named {1}. Should I continue?' format: { self pullRequest branchName })) ifFalse: [ ^ self ].

	IceGitHubAPI ensureCredentials.

	self pullRequest mergeIntoWorkingCopy
]

{ #category : #'instance creation' }
IceGitHubTipPullRequestBrowser >> newCommitsDataSource [
	| commitModels |
	commitModels := self pullRequest commits collect: [ :each | (IceTipCommitModel repositoryModel: self repositoryModel on: each) beCached ].
	^ IceTipDataSource new
		tool: self;
		elements: commitModels;
		yourself
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> newContext [
	^ IceTipPullRequestsContext for: self
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> newContextWithSelection: aCommitModel [
	^ self newContext
		item: aCommitModel;
		yourself
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> pullRequest [
	^ pullRequest
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> pullRequest: anObject [
	pullRequest := anObject
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> rejectButton [
	^ rejectButton
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> rejectButton: anObject [
	rejectButton := anObject
]

{ #category : #actions }
IceGitHubTipPullRequestBrowser >> rejectPullRequest [
	IceGitHubRejectPullRequestModel new
		onAccept: [ :model | 
			IceGitHubAPI ensureCredentials.
			self pullRequest rejectMessage: model message ];
		openWithSpec
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> repositoryModel [
	^ repositoryModel
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> repositoryModel: anObject [
	repositoryModel := anObject
]

{ #category : #'model access' }
IceGitHubTipPullRequestBrowser >> setModelBeforeInitialization: aModel [
	self repositoryModel: aModel
]

{ #category : #accessing }
IceGitHubTipPullRequestBrowser >> title [
	^ 'Browsing pull request: #{1} {2}' format: { self pullRequest number. self pullRequest title }
]

{ #category : #initialization }
IceGitHubTipPullRequestBrowser >> uptadeSelectedCommit: anEvent [
	self diff selectedModel: (anEvent newSelectedRowIndexes ifNotEmpty: [ :indexes | (self commits widget dataSource elementAt: indexes first)  ] ifEmpty: [ nil ])
]
