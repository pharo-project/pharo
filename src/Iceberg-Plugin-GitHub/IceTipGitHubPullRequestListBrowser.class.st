"
Description
--------------------

I am a spec presenter to show the list of PR available for a remote and informations on them.
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	details:				<aPresenter>		A presenten showing the details for a PR.
	pullRequests:		<anAdaptor>		An adaptor corresponding to a table containing the pull requests to view.
	remote:				<aRemote>			The remote we should view the PR of.

"
Class {
	#name : #IceTipGitHubPullRequestListBrowser,
	#superclass : #IceTipBrowser,
	#instVars : [
		'remote',
		'pullRequests',
		'details'
	],
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #specs }
IceTipGitHubPullRequestListBrowser class >> defaultSpec [
	^ SpecLayout composed
		newColumn: [ :mainColumn | 
			mainColumn
				newRow: #toolbar height: IceTipToolbar defaultHeight;
				newRow: #pullRequests height: 150;
				addHSplitter;
				newRow: #details ]
]

{ #category : #'instance creation' }
IceTipGitHubPullRequestListBrowser class >> onRepository: aRepository [
	^ self on: (IceTipRepositoryModel on: aRepository)
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser class >> taskbarIcon [
	^ self iconNamed: #komitterSmalltalkhubRemote
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser >> contextClass [
	^ IceTipPullRequestsListContext
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser >> details [
	^ details
]

{ #category : #private }
IceTipGitHubPullRequestListBrowser >> fetchPullRequests [
	^ (IceGitHubAPI new
		getPullRequests: self remote owner project: self remote projectName) all
		collect: [ :each | 
			(IceGitHubPullRequest fromJSON: each)
				repository: self repository entity;
				yourself ]
]

{ #category : #api }
IceTipGitHubPullRequestListBrowser >> initialExtent [
	^ (700@700) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipGitHubPullRequestListBrowser >> initializePresenter [
	super initializePresenter.
	self pullRequests widget onAnnouncement: FTSelectionChanged send: #uptadeSelectedPullRequest: to: self.
	self pullRequests widget onAnnouncement: FTStrongSelectionChanged send: #strongSelection: to: self.
]

{ #category : #initialization }
IceTipGitHubPullRequestListBrowser >> initializePullRequestsTable [
	pullRequests widget
		beResizable;
		addColumn:
			(IceTipTableColumn new
				id: '#';
				action: [ :pullRequest | pullRequest number asString ];
				width: 50 * World displayScaleFactor;
				yourself);
		addColumn:
			(IceTipTableColumn new
				id: 'State';
				action: #state;
				width: 50 * World displayScaleFactor;
				yourself);
		addColumn:
			(IceTipTableColumn new
				id: 'Author';
				action: #author;
				width: 120 * World displayScaleFactor;
				yourself);
		addColumn:
			(IceTipTableColumn new
				id: 'Title';
				action: #title;
				yourself);
		dataSource: self newPullRequestsDataSource
]

{ #category : #initialization }
IceTipGitHubPullRequestListBrowser >> initializeWidgets [
	super initializeWidgets.
	pullRequests := self newIceTable.
	details := self instantiate: IceGitHubPullRequestDetailModel.
	
	self initializePullRequestsTable.
	
	self focusOrder
		add: pullRequests;
		add: details
]

{ #category : #private }
IceTipGitHubPullRequestListBrowser >> newContextWithSelection: anObject [
	^ self newContext 
		item: anObject;
		yourself
]

{ #category : #initialization }
IceTipGitHubPullRequestListBrowser >> newPullRequestsDataSource [
	^ IceTipDataSource new 
		tool: self;
		elements: #();
		yourself
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser >> pullRequests [
	^ pullRequests
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser >> pullRequests: anObject [
	pullRequests := anObject
]

{ #category : #actions }
IceTipGitHubPullRequestListBrowser >> refreshPullRequests [
	| table |
	table := self pullRequests widget.
	table dataSource elements: self fetchPullRequests.
	(table hasSelection not and: [ table dataSource hasElements ])
		ifTrue: [ table selectRowIndex: 1 ]
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser >> remote [
	^ remote
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser >> remote: anObject [
	remote := anObject
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser >> repository [
	^ model
]

{ #category : #accessing }
IceTipGitHubPullRequestListBrowser >> selectedPullRequest [
	| table |
	table := self pullRequests widget.
	^ table dataSource elementAt: table selectedRowIndex 
]

{ #category : #'as yet unclassified' }
IceTipGitHubPullRequestListBrowser >> strongSelection: anEvent [
	self viewPullRequest: (self pullRequests widget dataSource elementAt: anEvent selectedRowIndex)
]

{ #category : #initialization }
IceTipGitHubPullRequestListBrowser >> title [
	^ 'Pull requests on {1} ({2})' format: { self remote name. self remote url}
]

{ #category : #'as yet unclassified' }
IceTipGitHubPullRequestListBrowser >> uptadeSelectedPullRequest: anEvent [
	self details pullRequest: (anEvent newSelectedRowIndexes ifNotEmpty: [ :indexes | self pullRequests widget dataSource elementAt: indexes first ] ifEmpty: [ IceGitHubNilPullRequest new ])
]

{ #category : #actions }
IceTipGitHubPullRequestListBrowser >> viewPullRequest: aPullRequest [
	UIManager default
		informUser:
			('Opening pull request {1} {2}'
				format:
					{aPullRequest number.
					aPullRequest title})
		during: [ "IceGitHubPullRequestBrowser new 
			pullRequest: aPullRequest;
			open" (IceGitHubTipPullRequestBrowser on: self model pullRequest: aPullRequest) openWithSpec ]
]
