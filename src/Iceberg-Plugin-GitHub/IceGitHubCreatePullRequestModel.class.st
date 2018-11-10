"
I'm a dialog to ask for data relevant to create a pull request.
"
Class {
	#name : #IceGitHubCreatePullRequestModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'repository',
		'titleLabel',
		'titlePanel',
		'headLabel',
		'headForkList',
		'headBranchList',
		'baseLabel',
		'baseForkList',
		'baseBranchList',
		'bodyLabel',
		'bodyPanel',
		'addButton',
		'acceptBlock',
		'availableRemotes',
		'availableBranchNames',
		'baseRemote',
		'gitRepositoryInfo',
		'credentials',
		'baseBranches',
		'headRemote'
	],
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #specs }
IceGitHubCreatePullRequestModel class >> defaultSpec [ 
	<spec>

	^ SpecLayout composed
		newColumn: [ :col |
			col 
				newRow: [:row | row add: #titleLabel width: 80 * World displayScaleFactor; add: #titlePanel ] height: 25 * World displayScaleFactor;
				newRow: [:row | 
					row 
						newColumn: #headLabel width: 80 * World displayScaleFactor;
						newColumn: [ :col2 | 
							col2 newRow: [ :row2 | 
								row2
									add: #headForkList;
									add: #headBranchList ] ] ] height: 25 * World displayScaleFactor;
				newRow: [:row | 
					row 
						newColumn: #baseLabel width: 80 * World displayScaleFactor;
						newColumn: [ :col2 |
							col2 newRow: [ :row2 |
								row2 
									add: #baseForkList;
									add: #baseBranchList ] ] ] height: 25 * World displayScaleFactor;
				newRow: [:row | row add: #bodyLabel width: 80 * World displayScaleFactor; add: #bodyPanel ];
				add: #addButton height: self buttonHeight  ];
		yourself
]

{ #category : #'instance creation' }
IceGitHubCreatePullRequestModel class >> new [
	self error: 'Use #repository:credentials:'
]

{ #category : #'instance creation' }
IceGitHubCreatePullRequestModel class >> repository: aRepository credentials: aCredentials headRemote: aRemote [
	^ self basicNew 
		initializeRepository: aRepository 
			credentials: aCredentials
			headRemote: aRemote;
		yourself
]

{ #category : #specs }
IceGitHubCreatePullRequestModel class >> title [
	^ 'New pull request'
]

{ #category : #actions }
IceGitHubCreatePullRequestModel >> accept [
	| title |
	
	title := self titlePanel text trim.
	title ifEmpty: [ 
		UIManager default alert: 'You need to specify a title for the pull request.' title: 'Error'. 
		^ self ].
	
	acceptBlock ifNotNil: [ 
		acceptBlock cull: self createPullRequest ].
	
	self window delete
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> addButton [
	^ addButton
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> addButton: anObject [
	addButton := anObject
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> availableBranchNames [
	^ availableBranchNames ifNil: [ availableBranchNames := self obtainAvailableBranchNames ]
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> availableRemotes [
	"retrieves available remotes to perform the PR"
	^ availableRemotes ifNil: [ availableRemotes := self obtainAvailableRemotes ]
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> baseBranchList [
	^ baseBranchList
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> baseBranchList: anObject [
	baseBranchList := anObject
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> baseBranchNames [
	^ self baseBranches collect: [ :each | each at: #name ]
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> baseBranches [
	^ baseBranches ifNil: [ 
		baseBranches := self requestBaseBranches ]
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> baseDefaultBranchName [
	^ self gitBase at: #'default_branch' 
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> baseForkList [
	^ baseForkList
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> baseForkList: anObject [
	baseForkList := anObject
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> baseLabel [
	^ baseLabel
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> baseLabel: anObject [
	baseLabel := anObject
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> baseRemote [
	"This is the 'base' remote (the place where the repository is forked). This is used to 
	 suggest fork PR destination (base)"
	^ baseRemote ifNil: [ baseRemote := self obtainBaseRemote ]
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> bodyLabel [
	^ bodyLabel
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> bodyLabel: anObject [
	bodyLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> bodyPanel [
	^ bodyPanel
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> bodyPanel: anObject [
	bodyPanel := anObject
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> branch [
	^ self repository branch
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> createPullRequest [
	^ IceGitHubPullRequestDefinition new 
		credentials: self credentials;
		title: self titlePanel text trim;
		headRemote: self headForkList selectedItem;
		headBranchName: self headBranchList selectedItem;
		baseRemote: self baseForkList selectedItem;
		baseBranchName: self baseBranchList selectedItem;
		body: self bodyPanel text asString trim;
		yourself
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> credentials [
	^ credentials
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> credentials: aCredentials [
	credentials := aCredentials
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> defaultBaseBranchName [
	self hasParent ifFalse: [ ^ self baseDefaultBranchName ].
	^ self baseBranchNames 
		detect: [ :each | each = self branch shortname ] 
		ifNone: [ self baseDefaultBranchName ]
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> defaultHeadBranchName [
	^ self branch shortname
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> gitBase [
	| info |
	^ (info := self gitRepositoryInfo) 
		at: #parent
		ifAbsent: [ info ]
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> gitRepositoryInfo [
	^ gitRepositoryInfo ifNil: [ 
		gitRepositoryInfo := self requestGitRepositoryInfo ]
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> hasParent [
	^ self gitRepositoryInfo includesKey: #parent
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> headBranchList [
	^ headBranchList
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> headBranchList: anObject [
	headBranchList := anObject
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> headForkList [
	^ headForkList
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> headForkList: anObject [
	headForkList := anObject
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> headLabel [
	^ headLabel
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> headLabel: anObject [
	headLabel := anObject
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> headRemote [ 
	^ headRemote ifNil: [ self repository head upstream remote ]
]

{ #category : #api }
IceGitHubCreatePullRequestModel >> initialExtent [
	^ (500@300) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceGitHubCreatePullRequestModel >> initializePresenter [
	super initializePresenter.
	self titlePanel text: self branch shortname.
	self headForkList
		items: self availableRemotes;
		displayBlock: [ :each | each owner, '/', each projectName ];
		setSelectedItem: self headRemote.
	self headBranchList 
		items: self availableBranchNames;
		displayBlock: [ :each | each ];
		setSelectedItem: self defaultHeadBranchName.
	self baseForkList
		items: self availableRemotes;
		displayBlock: [ :each | each owner, '/', each projectName ];
		setSelectedItem: self baseRemote.
	self baseBranchList 
		items: self availableBranchNames;
		displayBlock: [ :each | each ];
		setSelectedItem: self defaultBaseBranchName.
	self bodyPanel text: self obtainLastCommitMessage
]

{ #category : #initialization }
IceGitHubCreatePullRequestModel >> initializeRepository: aRepository credentials: aCredentials headRemote: aRemote [
	repository := aRepository.
	credentials := aCredentials.
	headRemote := aRemote.
	self initialize
]

{ #category : #initialization }
IceGitHubCreatePullRequestModel >> initializeWidgets [
	titleLabel := self newLabel.
	titlePanel := self newTextInput autoAccept: true.
	headLabel := self newLabel.
	headForkList := self newDropList.
	headBranchList := self newDropList.
	baseLabel := self newLabel.
	baseForkList := self newDropList.
	baseBranchList := self newDropList.
	bodyLabel := self newLabel.
	bodyPanel := self newText autoAccept: true.
	addButton := self newButton.
		
	titleLabel label: 'Title'.
	headLabel label: 'From (Head)'.
	baseLabel label: 'To (Base)'.
	bodyLabel label: 'Comment'.
	addButton label: 'Create pull request'.
	addButton action: [ self accept ].
	
	self focusOrder 
		add: titlePanel;
		add: headForkList;
		add: headBranchList;
		add: baseForkList;
		add: baseBranchList;
		add: bodyPanel;
		add: addButton
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> obtainAvailableBranchNames [
	^ ((self repository allBranches 
		collect: #shortname as: Set)
		addAll: self baseBranchNames;
		asArray)
		sort.
	
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> obtainAvailableRemotes [
	| remotes |
	
	remotes := self repository remotes.
	(remotes includes: self baseRemote)
		ifFalse: [ remotes := remotes copyWith: self baseRemote ].
	^ remotes	

]

{ #category : #private }
IceGitHubCreatePullRequestModel >> obtainBaseRemote [
	| base |
	base := self gitBase.
	^ IceGitRemote url: ((base at: #'git_url') copyReplaceAll: 'git://' with: 'https://')
]

{ #category : #private }
IceGitHubCreatePullRequestModel >> obtainLastCommitMessage [
	^ self branch commit comment
]

{ #category : #events }
IceGitHubCreatePullRequestModel >> onAccept: aBlock [
	acceptBlock := aBlock
]

{ #category : #accessing }
IceGitHubCreatePullRequestModel >> repository [
	^ repository
]

{ #category : #'private request' }
IceGitHubCreatePullRequestModel >> requestBaseBranches [
	^ IceGitHubAPI new 
		credentials: self credentials;
		getBranches: self baseRemote owner project: self baseRemote projectBasename 
]

{ #category : #'private request' }
IceGitHubCreatePullRequestModel >> requestGitRepositoryInfo [
	| remote |
	remote := self headRemote.
	^ IceGitHubAPI new 
		credentials: self credentials;
		getRepository: remote owner 
			project: remote projectBasename

]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> titleLabel [
	^ titleLabel
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> titleLabel: anObject [
	titleLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> titlePanel [
	^ titlePanel
]

{ #category : #'accessing ui' }
IceGitHubCreatePullRequestModel >> titlePanel: anObject [
	titlePanel := anObject
]
