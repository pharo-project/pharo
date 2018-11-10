"
I'm a base browser for pull/push action browser.
"
Class {
	#name : #IceTipHistoryActionBrowser,
	#superclass : #IceTipBrowser,
	#instVars : [
		'commitsPanel',
		'actionPanel',
		'button'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #actions }
IceTipHistoryActionBrowser >> accept [
	button performAction
]

{ #category : #'accessing ui' }
IceTipHistoryActionBrowser >> actionPanel [
	^ actionPanel
]

{ #category : #'accessing ui' }
IceTipHistoryActionBrowser >> actionPanel: anObject [
	actionPanel := anObject
]

{ #category : #'accessing ui' }
IceTipHistoryActionBrowser >> commitsPanel [
	^ commitsPanel
]

{ #category : #'accessing ui' }
IceTipHistoryActionBrowser >> commitsPanel: anObject [
	commitsPanel := anObject
]

{ #category : #accessing }
IceTipHistoryActionBrowser >> initialExtent [
	^ (800@600) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipHistoryActionBrowser >> initializeWidgets [
	super initializeWidgets.
	
	commitsPanel := self instantiate: IceTipHistoryBrowser on: self model.
	actionPanel := self newIceActionPanel.
	actionPanel addAction: (button := self newActionButton).
	
	self whenBuiltDo: [ :ann | self addShortcutsTo: ann widget ]
]

{ #category : #actions }
IceTipHistoryActionBrowser >> moveToRemote: aRemoteModel [
	aRemoteModel ifNil: [ ^ self ].
	
	self model useRemote: aRemoteModel.
	self model reset.
	self refreshCommits.
	self updateTitle
]

{ #category : #'private factory' }
IceTipHistoryActionBrowser >> newActionButton [
	^ self subclassResponsibility
]

{ #category : #accessing }
IceTipHistoryActionBrowser >> refresh [
	self refreshCommits.
]

{ #category : #accessing }
IceTipHistoryActionBrowser >> refreshCommits [
	commitsPanel refresh
]

{ #category : #private }
IceTipHistoryActionBrowser >> refreshWhenRepository: ann [
	(self model repositoryModel isModelOf: ann repository)
		ifFalse: [ ^ self ].
	self model reset.
	self refresh
]

{ #category : #initialization }
IceTipHistoryActionBrowser >> subscribeToAnnouncements [
	self announcer 
		when: IceTipDiffRefreshed 
		send: #refreshWhenRepository: 
		to: self.
		
	Iceberg announcer weak 
		when: IceRepositoryAnnouncement 
		send: #refreshWhenRepository: 
		to: self
]
