"
I'm a dialog to add remotes to a repository.
"
Class {
	#name : #IceTipAddRemoteDialog,
	#superclass : #IceTipDialog,
	#instVars : [
		'model',
		'nameLabel',
		'urlLabel',
		'urlText',
		'nameText',
		'acceptBlock'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipAddRemoteDialog class >> defaultSpec [ 
	<spec>

	^ SpecLayout composed
		newColumn: [ :column |
			column
				newRow: [ :row | 
					row 
						newColumn: #nameLabel width: self iceLabelWidth; 
						newColumn: #nameText ] 
					height: self inputTextHeight;
				newRow: [ :row | 
					row 
						newColumn: #urlLabel width: self iceLabelWidth; 
						newColumn: #urlText ] 
					height: self inputTextHeight ];
		yourself
]

{ #category : #specs }
IceTipAddRemoteDialog class >> title [
	^ 'Add remote'
]

{ #category : #actions }
IceTipAddRemoteDialog >> accept [
	self validate.
	self model 
		addRemoteNamed: self remoteName 
		url: self remoteUrl.
	^ acceptBlock ifNotNil: [ acceptBlock cull: self remoteName ]
]

{ #category : #accessing }
IceTipAddRemoteDialog >> initialExtent [
	^ (500@(
		self class inputTextHeight * 2
		+ self class buttonHeight 
		+ 50 "Yes, a magic number"
		)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipAddRemoteDialog >> initializeWidgets [
	nameLabel := self newLabel label: 'Remote name'. 
	nameText := self newTextInput 
		ghostText: 'e.g., user-remote'; 
		autoAccept: true.
	urlLabel := self newLabel label: 'Remote URL'.
	urlText := self newTextInput 
		ghostText: 'e.g., git@github.com:user/project.git'; 
		autoAccept: true.
	
	self focusOrder 
		add: nameText;
		add: urlText
]

{ #category : #accessing }
IceTipAddRemoteDialog >> model [
	^ model
]

{ #category : #accessing }
IceTipAddRemoteDialog >> model: anObject [
	model := anObject
]

{ #category : #'accessing ui' }
IceTipAddRemoteDialog >> nameLabel [
	^ nameLabel
]

{ #category : #'accessing ui' }
IceTipAddRemoteDialog >> nameLabel: anObject [
	nameLabel := anObject
]

{ #category : #'accessing ui' }
IceTipAddRemoteDialog >> nameText [
	^ nameText
]

{ #category : #'accessing ui' }
IceTipAddRemoteDialog >> nameText: anObject [
	nameText := anObject
]

{ #category : #events }
IceTipAddRemoteDialog >> onAccept: aBlock [
	acceptBlock := aBlock
]

{ #category : #accessing }
IceTipAddRemoteDialog >> remoteName [
	^ self nameText text trimmed
]

{ #category : #accessing }
IceTipAddRemoteDialog >> remoteUrl [
	^ self urlText text trimmed
]

{ #category : #initialization }
IceTipAddRemoteDialog >> setModelBeforeInitialization: anObject [ 
	model := anObject
]

{ #category : #'accessing ui' }
IceTipAddRemoteDialog >> urlLabel [
	^ urlLabel
]

{ #category : #'accessing ui' }
IceTipAddRemoteDialog >> urlLabel: anObject [
	urlLabel := anObject
]

{ #category : #'accessing ui' }
IceTipAddRemoteDialog >> urlText [
	^ urlText
]

{ #category : #'accessing ui' }
IceTipAddRemoteDialog >> urlText: anObject [
	urlText := anObject
]

{ #category : #actions }
IceTipAddRemoteDialog >> validate [
	self 
		assert: self remoteName notEmpty 
		description: 'You need to specify a remote name.'.
	self 
		assert: self remoteUrl notEmpty
		description: 'You need to specify a remote url.'
]
