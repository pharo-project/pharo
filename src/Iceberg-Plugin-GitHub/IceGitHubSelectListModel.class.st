"
I'm a dialog to select a list of anything (used by branches now).
"
Class {
	#name : #IceGitHubSelectListModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'selectedItems',
		'listPanel',
		'selectButton',
		'acceptBlock'
	],
	#category : #'Iceberg-Plugin-GitHub-View'
}

{ #category : #specs }
IceGitHubSelectListModel class >> buttonLabel [
	^ 'Select'
]

{ #category : #api }
IceGitHubSelectListModel class >> defaultExtent [ 
	^ 550@500
]

{ #category : #specs }
IceGitHubSelectListModel class >> defaultSpec [
	<spec>
	^ SpecLayout composed
		newColumn:[ :col | 
			col  
				newRow: #listPanel;
				newRow: #selectButton height: self buttonHeight ];
		yourself
]

{ #category : #specs }
IceGitHubSelectListModel class >> title [
	^ 'Select...'
]

{ #category : #actions }
IceGitHubSelectListModel >> confirmSelection [
	acceptBlock ifNotNil: [ 
		acceptBlock value: selectedItems ].
	self window delete
]

{ #category : #'private factory' }
IceGitHubSelectListModel >> createCheckboxFor: anObject [
	^ CheckboxMorph 
		on: (IceGitHubSelectItem list: selectedItems item: anObject) 
		selected: #value 
		changeSelected: #value:
]

{ #category : #api }
IceGitHubSelectListModel >> displayBlock: aBlock [ 
	self listPanel displayBlock: aBlock
]

{ #category : #api }
IceGitHubSelectListModel >> initialExtent [
	^ self class defaultExtent
]

{ #category : #initialization }
IceGitHubSelectListModel >> initialize [
	selectedItems := Set new.
	super initialize.
	self title: self class title
]

{ #category : #initialization }
IceGitHubSelectListModel >> initializeWidgets [
	listPanel := self instantiate: FastTablePresenter.
	listPanel icons: [ :each | self createCheckboxFor: each ].
	selectButton := self newButton.
	
	selectButton 
		label: self class buttonLabel;
		action: [ self confirmSelection ].
	
	self focusOrder 
		add: listPanel;
		add: selectButton
]

{ #category : #api }
IceGitHubSelectListModel >> items: aCollection [
	self listPanel items: aCollection
]

{ #category : #'accessing ui' }
IceGitHubSelectListModel >> listPanel [
	^ listPanel
]

{ #category : #'accessing ui' }
IceGitHubSelectListModel >> listPanel: anObject [
	listPanel := anObject
]

{ #category : #events }
IceGitHubSelectListModel >> onAccept: aBlock [
	acceptBlock := aBlock
]

{ #category : #'accessing ui' }
IceGitHubSelectListModel >> selectButton [
	^ selectButton
]

{ #category : #'accessing ui' }
IceGitHubSelectListModel >> selectButton: anObject [
	selectButton := anObject
]

{ #category : #api }
IceGitHubSelectListModel >> selectLabel: aString [
	self selectButton label: aString
]
