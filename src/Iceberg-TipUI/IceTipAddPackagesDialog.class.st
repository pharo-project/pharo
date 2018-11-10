"
I'm a dialog to add packages to a repository.
"
Class {
	#name : #IceTipAddPackagesDialog,
	#superclass : #IceTipDialog,
	#instVars : [
		'model',
		'packagesList',
		'packageNameText'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipAddPackagesDialog class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newColumn: [ :column |
			column 
				newRow: #packagesList;
				newRow: #packageNameText height: self inputTextHeight ]
]

{ #category : #accessing }
IceTipAddPackagesDialog class >> icon [
	^ self iconNamed: #branch
]

{ #category : #accessing }
IceTipAddPackagesDialog class >> order [
	^ 20
]

{ #category : #specs }
IceTipAddPackagesDialog class >> title [
	^ 'Add packages'
]

{ #category : #actions }
IceTipAddPackagesDialog >> accept [
	| items |
	
	"ensure window goes away (in case accept comes from a keybinding and not the ok button)"
	self window delete. 
	
	items := self packagesList widget dataSource selectedItems.
	items ifEmpty: [ 
		UIManager default inform: 'No packages selected.'.
		^ self ].
	
	self model addPackages: items
]

{ #category : #initialization }
IceTipAddPackagesDialog >> addShortcutsTo: aWidget [
	super addShortcutsTo: aWidget.
	aWidget 
		bindKeyCombination: Character cr asKeyCombination 
		toAction: [ self accept ].
	
]

{ #category : #accessing }
IceTipAddPackagesDialog >> allPackages [
	^ RPackageOrganizer default packages
		asOrderedCollection 
		sort: [ :a :b | a name < b name ]
]

{ #category : #accessing }
IceTipAddPackagesDialog >> allPackagesNotRegistered [
	| currentPackagesNames |
	currentPackagesNames := self model packages collect: #packageName.
	^ self allPackages reject: [ :rPackage | currentPackagesNames includes: rPackage name ]
]

{ #category : #accessing }
IceTipAddPackagesDialog >> contextSelection [ 
	^ self selectedPackage
]

{ #category : #initialization }
IceTipAddPackagesDialog >> initializeDialogWindow: aWindow [
	super initializeDialogWindow: aWindow.
	acceptButton
		disable;
		label: 'Add'
]

{ #category : #initialization }
IceTipAddPackagesDialog >> initializePackageNameText [
	self packageNameText whenBuiltDo: [ :ann |
		ann widget 
			bindKeyCombination: Character enter asKeyCombination 
				toAction: [ self accept ];
			bindKeyCombination: Character arrowUp asKeyCombination 
				toAction: [ 
					self packagesList takeKeyboardFocus.
					self packagesList widget selectLastVisibleRow ];
			bindKeyCombination: Character arrowDown asKeyCombination 
				toAction: [ 
					self packagesList takeKeyboardFocus.
					self packagesList widget selectFirstVisibleRow ] ]
]

{ #category : #initialization }
IceTipAddPackagesDialog >> initializePackagesList [
	| dataSource |
	
	dataSource := self newPackagesListDataSource.
	packagesList widget
		addColumn: (IceTipTableColumn newAction: #name icon: [ :each | self iconNamed: #package ]);
		dataSource: dataSource;
		useFunction: IceTipLinkFilterFunction do: [ :function | 
			function 
				filterClass: IceTipPackageFilter;
				textField: self packageNameText ];
		bindKeyCombination: Character cr toAction: [ self accept ];
		onAnnouncement: FTStrongSelectionChanged do: [ self accept ]
]

{ #category : #initialization }
IceTipAddPackagesDialog >> initializeWidgets [
	packagesList := self newIceTreeTable.
	packageNameText := self newTextInput 
		autoAccept: true;
		ghostText: 'e.g., Collections'.
	
	self initializePackagesList.
	self initializePackageNameText.
	
	self focusOrder 
		add: self packagesList;
		add: self packageNameText
]

{ #category : #accessing }
IceTipAddPackagesDialog >> model [
	^ model
]

{ #category : #accessing }
IceTipAddPackagesDialog >> model: anObject [
	model := anObject.
	self refresh
]

{ #category : #'private factory' }
IceTipAddPackagesDialog >> newPackagesListDataSource [
	| dataSource |
	^ (dataSource := IceTipSelectingDataSource new)
		tool: self;
		elements: self allPackagesNotRegistered;
		onSelectionChangedBlock: [ :item :wasSelected | wasSelected ifTrue: [ acceptButton enable ] ifFalse: [ dataSource selectedItems ifEmpty: [ acceptButton disable ] ] ];
		yourself
]

{ #category : #accessing }
IceTipAddPackagesDialog >> packageNameText [
	^ packageNameText
]

{ #category : #accessing }
IceTipAddPackagesDialog >> packageNameText: anObject [
	packageNameText := anObject
]

{ #category : #accessing }
IceTipAddPackagesDialog >> packagesList [
	^ packagesList
]

{ #category : #accessing }
IceTipAddPackagesDialog >> packagesList: anObject [
	packagesList := anObject
]

{ #category : #accessing }
IceTipAddPackagesDialog >> refresh [
	packagesList widget 
		in: [ :this | 
			this selectRowIndexes: #(). 
			this dataSource 
				rootForItems: self model repositoryModelsByGroup;
				expandAll ];
		refresh.
]

{ #category : #accessing }
IceTipAddPackagesDialog >> selectedPackage [
	| index |
	self flag: #todo. "This should -maybe- be modeled better on a SpecTablePresenter ;)"
	index := packagesList widget selectedRowIndex.
	index = 0 ifTrue: [ ^ nil ].
	^ packagesList widget dataSource elementAt: index
]

{ #category : #initialization }
IceTipAddPackagesDialog >> setModelBeforeInitialization: anObject [ 
	model := anObject
]

{ #category : #accessing }
IceTipAddPackagesDialog >> validate [
	self 
		assert: self selectedPackage isNotNil 
		description: 'Please select a branch to checkout.'
]
