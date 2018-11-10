"
I'm a dialog for collecting data to accept a pull request.
"
Class {
	#name : #IceGitHubAcceptPullRequestModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'messageLabel',
		'messageText',
		'typeLabel',
		'typeList',
		'acceptButton',
		'acceptBlock'
	],
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #accessing }
IceGitHubAcceptPullRequestModel class >> availableTypes [
	^ #('merge' 'squash' 'rebase')
]

{ #category : #specs }
IceGitHubAcceptPullRequestModel class >> defaultSpec [ 
	<spec>

	^ SpecLayout composed
		newColumn: [ :col |
			col 
				newRow: [:row | row add: #messageLabel width: 80 * World displayScaleFactor; add: #messageText ];
				newRow: [:row | row add: #typeLabel width: 80 * World displayScaleFactor; add: #typeList ] height: 25 * World displayScaleFactor;
				add: #acceptButton height: self buttonHeight  ];
		yourself
]

{ #category : #accessing }
IceGitHubAcceptPullRequestModel class >> defaultType [
	^ 'merge'
]

{ #category : #accessing }
IceGitHubAcceptPullRequestModel class >> messageGhostText [
	^ 'Commit title

Add an optional extented description'
]

{ #category : #specs }
IceGitHubAcceptPullRequestModel class >> title [
	^ 'Accept pull request'
]

{ #category : #actions }
IceGitHubAcceptPullRequestModel >> accept [
	acceptBlock ifNotNil: [ 
		acceptBlock cull: self ].
	
	self window delete
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> acceptButton [
	^ acceptButton
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> acceptButton: anObject [
	acceptButton := anObject
]

{ #category : #api }
IceGitHubAcceptPullRequestModel >> initialExtent [
	^ (500@250) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceGitHubAcceptPullRequestModel >> initializePresenter [
	super initializePresenter.
	self typeList 
		items: self class availableTypes;
		displayBlock: [ :each | each  ];
		setSelectedItem: self class defaultType
]

{ #category : #initialization }
IceGitHubAcceptPullRequestModel >> initializeWidgets [
	messageLabel := self newLabel.
	messageText := self newText autoAccept: true.
	typeLabel := self newLabel.
	typeList := self newDropList.
	acceptButton := self newButton.
		
	messageLabel label: 'Comment'.
	messageText 	whenBuiltDo: [ :this | this widget withGhostText: self class messageGhostText ].
	typeLabel label: 'Merge type'.
	acceptButton label: 'Accept pull request'.
	acceptButton action: [ self accept ].
	
	self focusOrder 
		add: messageText;
		add: typeList;
		add: acceptButton
]

{ #category : #accessing }
IceGitHubAcceptPullRequestModel >> message [
	^ self messageText text asString
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> messageLabel [
	^ messageLabel
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> messageLabel: anObject [
	messageLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> messageText [
	^ messageText
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> messageText: anObject [
	messageText := anObject
]

{ #category : #events }
IceGitHubAcceptPullRequestModel >> onAccept: aBlock [
	acceptBlock := aBlock
]

{ #category : #accessing }
IceGitHubAcceptPullRequestModel >> type [
	^ self typeList selectedItem
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> typeLabel [
	^ typeLabel
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> typeLabel: anObject [
	typeLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> typeList [
	^ typeList
]

{ #category : #'accessing ui' }
IceGitHubAcceptPullRequestModel >> typeList: anObject [
	typeList := anObject
]
