Class {
	#name : #IceTipRemoveRepositoryDialog,
	#superclass : #IceTipDialog,
	#instVars : [
		'model',
		'iconPanel',
		'confirmLabel',
		'removeFileSystemCheckbox'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #accessing }
IceTipRemoveRepositoryDialog class >> confirmMessage [
	^ 'Are you sure to remove your repository {1} from your image?'
]

{ #category : #specs }
IceTipRemoveRepositoryDialog class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newRow: [ :row |
			row 
				newColumn: #iconPanel width: 40 * World displayScaleFactor;
				newColumn: [ :column |
					column 
						newRow: #confirmLabel;
						newRow: #removeFileSystemCheckbox height: self inputTextHeight ] ]
]

{ #category : #accessing }
IceTipRemoveRepositoryDialog class >> removeMessage [
	^ 'Also remove repository from file system'
]

{ #category : #specs }
IceTipRemoveRepositoryDialog class >> title [
	^ 'Remove repository'
]

{ #category : #actions }
IceTipRemoveRepositoryDialog >> accept [
	self removeFileSystemCheckbox state 
		ifTrue: [ self model delete ]
		ifFalse: [ self model forget ]
]

{ #category : #'accessing ui' }
IceTipRemoveRepositoryDialog >> confirmLabel [
	^ confirmLabel
]

{ #category : #'accessing ui' }
IceTipRemoveRepositoryDialog >> confirmLabel: anObject [
	confirmLabel := anObject
]

{ #category : #'accessing ui' }
IceTipRemoveRepositoryDialog >> iconPanel [
	^ iconPanel
]

{ #category : #'accessing ui' }
IceTipRemoveRepositoryDialog >> iconPanel: anObject [
	iconPanel := anObject
]

{ #category : #accessing }
IceTipRemoveRepositoryDialog >> initialExtent [
	^ (500@(
		self class inputTextHeight * 2
		+ self class buttonHeight 
		+ 50 "Yes, a magic number"
		)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipRemoveRepositoryDialog >> initializeWidgets [
	iconPanel := self newIcePanel.
	iconPanel widget addMorph: (self iconNamed: #question) asMorph.
	
	confirmLabel := self newLabel 
		label: (self class confirmMessage format: { self model name }).
	removeFileSystemCheckbox := self newCheckBox
		label: self class removeMessage.
		
	self focusOrder 
		add: self removeFileSystemCheckbox
]

{ #category : #accessing }
IceTipRemoveRepositoryDialog >> model [
	^ model
]

{ #category : #accessing }
IceTipRemoveRepositoryDialog >> model: anObject [
	model := anObject
]

{ #category : #'accessing ui' }
IceTipRemoveRepositoryDialog >> removeFileSystemCheckbox [
	^ removeFileSystemCheckbox
]

{ #category : #'accessing ui' }
IceTipRemoveRepositoryDialog >> removeFileSystemCheckbox: anObject [
	removeFileSystemCheckbox := anObject
]

{ #category : #initialization }
IceTipRemoveRepositoryDialog >> setModelBeforeInitialization: anObject [ 
	model := anObject
]
