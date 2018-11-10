Class {
	#name : #IceTipSelectRemotePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'remoteLabel',
		'remoteList',
		'addButton',
		'model'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipSelectRemotePresenter class >> defaultSpec [ 
	<spec>
	
	^ SpecLayout composed
		newRow: [ :row | 
			row 
				add: #remoteLabel width: self iceLabelWidth; 
			 	add: #remoteList; 
				add: #addButton width: self iceIconWidth ]
]

{ #category : #'accessing ui' }
IceTipSelectRemotePresenter >> addButton [
	^ addButton
]

{ #category : #'accessing ui' }
IceTipSelectRemotePresenter >> addButton: anObject [
	addButton := anObject
]

{ #category : #actions }
IceTipSelectRemotePresenter >> addRemote [
	(IceTipAddRemoteDialog on: self model repositoryModel) 
		onAccept: [ :newRemoteName | 
			self model reset.
			self refresh.
			self selectRemoteNamed: newRemoteName ];
		openDialogWithSpec.
]

{ #category : #accessing }
IceTipSelectRemotePresenter >> icon [
	^ self iconNamed: #add
]

{ #category : #initialization }
IceTipSelectRemotePresenter >> initializeWidgets [
	remoteLabel := self newLabel label: 'Remote:'.
	remoteList := self newDropList.
	remoteList
		items: self model remoteModels;
		displayBlock: [ :each | each descriptionWithDecoration ].
		
	addButton := self newButton icon: self icon.
	addButton action: [ self addRemote ].
	
	remoteList setSelectedItem: self model remoteModel.
	
	self focusOrder 
		add: remoteList
]

{ #category : #accessing }
IceTipSelectRemotePresenter >> label: aString [ 
	self remoteLabel label: aString
]

{ #category : #accessing }
IceTipSelectRemotePresenter >> model [
	^ model
]

{ #category : #accessing }
IceTipSelectRemotePresenter >> refresh [
	remoteList items: self model remoteModels.
]

{ #category : #'accessing ui' }
IceTipSelectRemotePresenter >> remoteLabel [
	^ remoteLabel
]

{ #category : #'accessing ui' }
IceTipSelectRemotePresenter >> remoteLabel: anObject [
	remoteLabel := anObject
]

{ #category : #'accessing ui' }
IceTipSelectRemotePresenter >> remoteList [
	^ remoteList
]

{ #category : #'accessing ui' }
IceTipSelectRemotePresenter >> remoteList: anObject [
	remoteList := anObject
]

{ #category : #accessing }
IceTipSelectRemotePresenter >> selectRemoteNamed: aString [
	remoteList setSelectedItem: (self model remoteModels detect: [ :each | each name = aString ])
]

{ #category : #initialization }
IceTipSelectRemotePresenter >> setModelBeforeInitialization: anObject [ 
	model := anObject
]
