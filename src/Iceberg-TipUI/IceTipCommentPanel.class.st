"
I'm a comment panel.
I can obtain a comment text and certain ""commit specific"" options. 
"
Class {
	#name : #IceTipCommentPanel,
	#superclass : #IceTipComposablePresenter,
	#instVars : [
		'repositoryModel',
		'commentText',
		'actionPanel',
		'pushCheckbox',
		'commitButton',
		'messageText',
		'message'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipCommentPanel class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column |
			column 
				newRow: #pushCheckbox height: self inputTextHeight;
				newRow: #commentText;
				newRow: #actionPanel height: self iceActionPanelHeight ]
]

{ #category : #specs }
IceTipCommentPanel class >> title [
	^ 'Comment'
]

{ #category : #'accessing ui' }
IceTipCommentPanel >> actionPanel [
	^ actionPanel
]

{ #category : #'accessing ui' }
IceTipCommentPanel >> actionPanel: anObject [
	actionPanel := anObject
]

{ #category : #accessing }
IceTipCommentPanel >> clear [
	commentText text: ''
]

{ #category : #'accessing ui' }
IceTipCommentPanel >> commentText [
	^ commentText
]

{ #category : #'accessing ui' }
IceTipCommentPanel >> commentText: anObject [
	commentText := anObject
]

{ #category : #'accessing ui' }
IceTipCommentPanel >> commitButton [
	^ commitButton
]

{ #category : #'accessing ui' }
IceTipCommentPanel >> commitButton: anObject [
	^ commitButton := anObject
]

{ #category : #initialization }
IceTipCommentPanel >> initialize [
	super initialize.
	messageText := '' asText
]

{ #category : #initialization }
IceTipCommentPanel >> initializeWidgets [

	pushCheckbox := self newCheckBox.
	self repositoryModel branchModel hasUpstream
		ifTrue: [ pushCheckbox label: ('Push changes to {1}' format: { self repositoryRemoteName }) ]
		ifFalse: [ pushCheckbox
				label: ('Cannot push new branch automatically. Use the `Push` option');
				enabled: false ].

	commentText := self newText
		autoAccept: true;
		whenBuiltDo: [ :ann |  ann widget withGhostText: 'A comment for your commit' ].
	actionPanel := self newIceActionPanel.
	actionPanel addAction: (commitButton := self newButton 
		label: 'Commit';
		icon: (self iconNamed: #smallOk);
		help: ('Commit your changes to {1}' format: { self repositoryName });
		yourself).
		
	self focusOrder
		add: commentText;
		add: commitButton;
		add: pushCheckbox
]

{ #category : #testing }
IceTipCommentPanel >> isPushing [
	^ self pushCheckbox state
]

{ #category : #accessing }
IceTipCommentPanel >> message [
	^ self commentText text asString
]

{ #category : #events }
IceTipCommentPanel >> onCommit: aBlock [
	commitButton action: [ 
		aBlock 
			cull: self message
			cull: self isPushing ]
]

{ #category : #'accessing ui' }
IceTipCommentPanel >> pushCheckbox [
	^ pushCheckbox
]

{ #category : #'accessing ui' }
IceTipCommentPanel >> pushCheckbox: anObject [
	pushCheckbox := anObject
]

{ #category : #accessing }
IceTipCommentPanel >> refresh [
	commitButton help: ('Commit your changes to {1}' format: { self repositoryName }).
	pushCheckbox label: ('Push changes to {1}' format: { self repositoryRemoteName })
]

{ #category : #accessing }
IceTipCommentPanel >> repositoryModel [
	^ repositoryModel
]

{ #category : #'private accessing' }
IceTipCommentPanel >> repositoryName [
	^ self repositoryModel name
]

{ #category : #'private accessing' }
IceTipCommentPanel >> repositoryRemoteName [
	^ self repositoryModel pushRemoteName
]

{ #category : #initialization }
IceTipCommentPanel >> setModelBeforeInitialization: anObject [ 
	repositoryModel := anObject
]
