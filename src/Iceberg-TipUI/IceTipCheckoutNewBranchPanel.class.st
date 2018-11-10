"
I'm a panel to create a new local branch 
"
Class {
	#name : #IceTipCheckoutNewBranchPanel,
	#superclass : #IceTipBranchPanel,
	#instVars : [
		'currentBranchLabel',
		'branchLabel',
		'branchInputText',
		'spacePanel',
		'branchCommitish',
		'callback'
	],
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #specs }
IceTipCheckoutNewBranchPanel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed 
		newColumn: [ :col | 
			col
				newRow: #currentBranchLabel height: self inputTextHeight;
				newRow: [ :row | 
						row 
							add: #branchLabel width: self iceLabelWidth;
							add: #branchInputText ]
					height: self inputTextHeight;
				newRow: #spacePanel ]
]

{ #category : #accessing }
IceTipCheckoutNewBranchPanel class >> icon [
	^ self iconNamed: #smallNew
]

{ #category : #accessing }
IceTipCheckoutNewBranchPanel class >> order [
	^ 10
]

{ #category : #specs }
IceTipCheckoutNewBranchPanel class >> title [
	^ 'New branch'
]

{ #category : #'accessing ui' }
IceTipCheckoutNewBranchPanel >> branchInputText [
	^ branchInputText
]

{ #category : #'accessing ui' }
IceTipCheckoutNewBranchPanel >> branchInputText: anObject [
	branchInputText := anObject
]

{ #category : #'accessing ui' }
IceTipCheckoutNewBranchPanel >> branchLabel [
	^ branchLabel
]

{ #category : #'accessing ui' }
IceTipCheckoutNewBranchPanel >> branchLabel: anObject [
	branchLabel := anObject
]

{ #category : #accessing }
IceTipCheckoutNewBranchPanel >> branchName [
	^ self branchInputText text asString trimBoth
]

{ #category : #accessing }
IceTipCheckoutNewBranchPanel >> commitish: anIceTipCachedModel [
	
	branchCommitish := anIceTipCachedModel
]

{ #category : #'accessing ui' }
IceTipCheckoutNewBranchPanel >> currentBranchLabel [
	^ currentBranchLabel
]

{ #category : #'accessing ui' }
IceTipCheckoutNewBranchPanel >> currentBranchLabel: anObject [
	currentBranchLabel := anObject
]

{ #category : #actions }
IceTipCheckoutNewBranchPanel >> doAccept [
	self validate.
	self model
		checkoutNewBranchNamed: self branchName
		inCommit: (branchCommitish ifNil: [ self model entity workingCopy referenceCommit ])
		thenDo: [ :newBranch | callback ifNotNil: [ callback value: newBranch ] ]
]

{ #category : #initialization }
IceTipCheckoutNewBranchPanel >> initializeCurrentBranchLabel [
	currentBranchLabel := self newLabel
		label: 'Current branch: ' , self model branchName
]

{ #category : #initialization }
IceTipCheckoutNewBranchPanel >> initializeFocusOrder [
	self focusOrder add: self branchInputText
]

{ #category : #initialization }
IceTipCheckoutNewBranchPanel >> initializeWidgets [
	self initializeWidgetsContents.
	self initializeFocusOrder
]

{ #category : #initialization }
IceTipCheckoutNewBranchPanel >> initializeWidgetsContents [
	self initializeCurrentBranchLabel.
	branchLabel := self newLabel label: 'New branch'.
	branchInputText := self newTextInput
		autoAccept: true;
		ghostText: 'e.g., feature/what'.
	spacePanel := PanelMorph new asSpecAdapter
]

{ #category : #testing }
IceTipCheckoutNewBranchPanel >> isNewBranch [
	^ true
]

{ #category : #accessing }
IceTipCheckoutNewBranchPanel >> openDialogWithSpec [
	^ super openDialogWithSpec
		okAction: [ self accept ];
		yourself
]

{ #category : #'accessing ui' }
IceTipCheckoutNewBranchPanel >> spacePanel [
	^ spacePanel
]

{ #category : #'accessing ui' }
IceTipCheckoutNewBranchPanel >> spacePanel: anObject [
	spacePanel := anObject
]

{ #category : #accessing }
IceTipCheckoutNewBranchPanel >> validate [
	self 
		assert: self branchName isNotEmpty
		description: 'Please enter your new branch name.'.
	self 
		assert: (self branchName noneSatisfy: #isSeparator)
		description: 'You can''t have spaces in your branch name.'
]

{ #category : #callbacks }
IceTipCheckoutNewBranchPanel >> withResultDo: aBlockClosure [ 
	
	callback := aBlockClosure
]
