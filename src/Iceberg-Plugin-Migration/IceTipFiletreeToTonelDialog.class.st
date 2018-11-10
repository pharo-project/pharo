"
I'm a dialog validate and perform a migration of sources from filetree to tonel.

"
Class {
	#name : #IceTipFiletreeToTonelDialog,
	#superclass : #IceTipDialog,
	#instVars : [
		'model',
		'iconPanel',
		'confirmLabel',
		'mergeWithMasterCheckbox',
		'branchName',
		'repository'
	],
	#category : #'Iceberg-Plugin-Migration'
}

{ #category : #accessing }
IceTipFiletreeToTonelDialog class >> branchBaseName [
	^ 'migrate-sources-to-tonel'
]

{ #category : #accessing }
IceTipFiletreeToTonelDialog class >> confirmMessage [
	^ 'Convert repository {1} sources from filetree format to tonel. 
This process will use a new branch named {2}. 
You can choose to not merge immediately your migration.

ARE YOU SURE?'
]

{ #category : #specs }
IceTipFiletreeToTonelDialog class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newRow: [ :row |
			row 
				newColumn: #iconPanel width: 40 * World displayScaleFactor;
				newColumn: [ :column |
					column 
						newRow: #confirmLabel;
						newRow: #mergeWithMasterCheckbox height: self inputTextHeight] ]
]

{ #category : #specs }
IceTipFiletreeToTonelDialog class >> title [
	^ 'Convert sources to tonel'
]

{ #category : #actions }
IceTipFiletreeToTonelDialog >> accept [
	
	IceTipStandardAction new 
		repository: self repository; 
		message: ('Migrating {1} sources to tonel' format: { self repository name });
		onSuccessRepositoryModified;
		onSuccess: [ UIManager default inform: 'Sources migrated!' ];
		execute: [ self doAccept ]
]

{ #category : #accessing }
IceTipFiletreeToTonelDialog >> branchName [
	^ branchName ifNil: [ branchName := self obtainBranchName ]
]

{ #category : #'accessing ui' }
IceTipFiletreeToTonelDialog >> confirmLabel [
	^ confirmLabel
]

{ #category : #'accessing ui' }
IceTipFiletreeToTonelDialog >> confirmLabel: anObject [
	confirmLabel := anObject
]

{ #category : #actions }
IceTipFiletreeToTonelDialog >> doAccept [
	
	IceConvertFormatAction new 
		repository: self repository;
		branchName: self branchName;
		format: #tonel;
		mergeWithCurrentBranch: self mergeWithMasterCheckbox state;
		execute
]

{ #category : #'accessing ui' }
IceTipFiletreeToTonelDialog >> iconPanel [
	^ iconPanel
]

{ #category : #'accessing ui' }
IceTipFiletreeToTonelDialog >> iconPanel: anObject [
	iconPanel := anObject
]

{ #category : #api }
IceTipFiletreeToTonelDialog >> initialExtent [
	^ (500@(
		self class inputTextHeight * 5
		+ self class buttonHeight 
		+ 50
		)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipFiletreeToTonelDialog >> initializeWidgets [
	iconPanel := self newIcePanel.
	iconPanel widget addMorph: (self iconNamed: #question) asMorph.
	
	confirmLabel := self newIceReadOnlyText. 
	confirmLabel widget contentsWrapped: (self class confirmMessage 
		format: { 
			self repository name. 
			self branchName }).
	mergeWithMasterCheckbox := self newCheckBox
		label: ('Merge with branch {1}' format: { self repository branchName });
		state: true.

	self focusOrder 
		add: mergeWithMasterCheckbox
]

{ #category : #'accessing ui' }
IceTipFiletreeToTonelDialog >> mergeWithMasterCheckbox [
	^ mergeWithMasterCheckbox
]

{ #category : #'accessing ui' }
IceTipFiletreeToTonelDialog >> mergeWithMasterCheckbox: anObject [
	mergeWithMasterCheckbox := anObject
]

{ #category : #private }
IceTipFiletreeToTonelDialog >> obtainBranchName [
	| baseName suffix count |
	
	count := 0. 	
	baseName := self class branchBaseName.
	suffix := ''.
	[ true ] whileTrue: [ 
		self repository 
			branchNamed: baseName, suffix
			ifAbsent: [ ^ baseName, suffix ].
		count := count + 1.
		suffix := '-', count asString ]
]

{ #category : #accessing }
IceTipFiletreeToTonelDialog >> repository [
	^ repository
]

{ #category : #initialization }
IceTipFiletreeToTonelDialog >> setModelBeforeInitialization: anObject [
	repository := anObject
]
