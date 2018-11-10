Class {
	#name : #IceTipRemovePackageDialog,
	#superclass : #IceTipDialog,
	#instVars : [
		'model',
		'iconPanel',
		'confirmLabel',
		'removeFromRepositoryCheckbox',
		'unloadFromImageCheckbox'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #accessing }
IceTipRemovePackageDialog class >> confirmMessage [
	^ 'Are you sure to remove package {1}?'
]

{ #category : #specs }
IceTipRemovePackageDialog class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newRow: [ :row |
			row 
				newColumn: #iconPanel width: 40 * World displayScaleFactor;
				newColumn: [ :column |
					column 
						newRow: #confirmLabel;
						newRow: #unloadFromImageCheckbox height: self inputTextHeight;
						newRow: #removeFromRepositoryCheckbox height: self inputTextHeight ] ]
]

{ #category : #specs }
IceTipRemovePackageDialog class >> title [
	^ 'Remove Package'
]

{ #category : #actions }
IceTipRemovePackageDialog >> accept [
	self removeFromRepositoryCheckbox state
		ifTrue: [ self model remove ].
	self unloadFromImageCheckbox state
		ifTrue: [ self model unload ].
]

{ #category : #'accessing ui' }
IceTipRemovePackageDialog >> confirmLabel [
	^ confirmLabel
]

{ #category : #'accessing ui' }
IceTipRemovePackageDialog >> confirmLabel: anObject [
	confirmLabel := anObject
]

{ #category : #'accessing ui' }
IceTipRemovePackageDialog >> iconPanel [
	^ iconPanel
]

{ #category : #'accessing ui' }
IceTipRemovePackageDialog >> iconPanel: anObject [
	iconPanel := anObject
]

{ #category : #accessing }
IceTipRemovePackageDialog >> initialExtent [
	^ (500@(
		self class inputTextHeight * 2
		+ self class buttonHeight 
		+ 50 "Yes, a magic number"
		)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipRemovePackageDialog >> initializeWidgets [
	iconPanel := self newIcePanel.
	iconPanel widget addMorph: (self iconNamed: #question) asMorph.
	
	confirmLabel := self newLabel 
		label: (self class confirmMessage format: { self model name }).
	unloadFromImageCheckbox := self newCheckBox
		label: 'Unload from image';
		state: true.
	removeFromRepositoryCheckbox := self newCheckBox
		label: 'Remove from repository';
		state: true.

	self focusOrder 
		add: unloadFromImageCheckbox;
		add: removeFromRepositoryCheckbox
]

{ #category : #accessing }
IceTipRemovePackageDialog >> model [
	^ model
]

{ #category : #accessing }
IceTipRemovePackageDialog >> model: anObject [
	model := anObject
]

{ #category : #accessing }
IceTipRemovePackageDialog >> removeFromRepositoryCheckbox [
	^ removeFromRepositoryCheckbox
]

{ #category : #accessing }
IceTipRemovePackageDialog >> removeFromRepositoryCheckbox: anObject [
	removeFromRepositoryCheckbox := anObject
]

{ #category : #initialization }
IceTipRemovePackageDialog >> setModelBeforeInitialization: anObject [ 
	model := anObject
]

{ #category : #accessing }
IceTipRemovePackageDialog >> unloadFromImageCheckbox [
	^ unloadFromImageCheckbox
]

{ #category : #accessing }
IceTipRemovePackageDialog >> unloadFromImageCheckbox: anObject [
	unloadFromImageCheckbox := anObject
]
