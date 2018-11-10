"
I'm a dialog to collect data about a pull request rejection.
"
Class {
	#name : #IceGitHubRejectPullRequestModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'messageLabel',
		'messageText',
		'acceptButton',
		'acceptBlock'
	],
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #accessing }
IceGitHubRejectPullRequestModel class >> availableTypes [
	^ #('merge' 'squash' 'rebase')
]

{ #category : #specs }
IceGitHubRejectPullRequestModel class >> defaultSpec [ 
	<spec>

	^ SpecLayout composed
		newColumn: [ :col |
			col 
				newRow: [:row | row add: #messageLabel width: 80 * World displayScaleFactor; add: #messageText ];
				add: #acceptButton height: self buttonHeight  ];
		yourself
]

{ #category : #accessing }
IceGitHubRejectPullRequestModel class >> defaultType [
	^ 'merge'
]

{ #category : #accessing }
IceGitHubRejectPullRequestModel class >> messageGhostText [
	^ 'Commit title

Add an optional extented description'
]

{ #category : #specs }
IceGitHubRejectPullRequestModel class >> title [
	^ 'Accept pull request'
]

{ #category : #actions }
IceGitHubRejectPullRequestModel >> accept [
	acceptBlock ifNotNil: [ 
		acceptBlock cull: self ].
	
	self window delete
]

{ #category : #'accessing ui' }
IceGitHubRejectPullRequestModel >> acceptButton [
	^ acceptButton
]

{ #category : #'accessing ui' }
IceGitHubRejectPullRequestModel >> acceptButton: anObject [
	acceptButton := anObject
]

{ #category : #api }
IceGitHubRejectPullRequestModel >> initialExtent [
	^ (500@250) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceGitHubRejectPullRequestModel >> initializeWidgets [
	messageLabel := self newLabel.
	messageText := self newText autoAccept: true.
	acceptButton := self newButton.
		
	messageLabel label: 'Comment'.
	messageText 	whenBuiltDo: [ :this | this widget withGhostText: self class messageGhostText ].
	acceptButton label: 'Reject pull request'.
	acceptButton action: [ self accept ].
	
	self focusOrder 
		add: messageText;
		add: acceptButton
]

{ #category : #accessing }
IceGitHubRejectPullRequestModel >> message [
	^ self messageText text asString
]

{ #category : #'accessing ui' }
IceGitHubRejectPullRequestModel >> messageLabel [
	^ messageLabel
]

{ #category : #'accessing ui' }
IceGitHubRejectPullRequestModel >> messageLabel: anObject [
	messageLabel := anObject
]

{ #category : #'accessing ui' }
IceGitHubRejectPullRequestModel >> messageText [
	^ messageText
]

{ #category : #'accessing ui' }
IceGitHubRejectPullRequestModel >> messageText: anObject [
	messageText := anObject
]

{ #category : #events }
IceGitHubRejectPullRequestModel >> onAccept: aBlock [
	acceptBlock := aBlock
]
