Class {
	#name : 'StAbstractSelectionPresenter',
	#superclass : 'StItemsSelectionPresenter',
	#instVars : [
		'dropList',
		'dropLabel',
		'acceptBlock'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'layout' }
StAbstractSelectionPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		add: #dropLabel withConstraints: [ :c | c height: self labelHeight ];
		add:
			(SpBoxLayout newLeftToRight
				add: #dropList ;
				yourself) withConstraints: [ :c | c height: self toolbarHeight  ];
		add: #label withConstraints: [ :c | c height: self buttonHeight ];
		add:
			(SpBoxLayout newLeftToRight
				add: #table;
				yourself);
		yourself
]

{ #category : 'specs' }
StAbstractSelectionPresenter class >> label: aString dropLabel: aString2 withItems: items selecting: selectedItems dropItems: dropItems acceptBlock: aBlock [
	^ self new
		label: aString
		dropLabel: aString2
		withItems: items
		selecting: selectedItems
		dropItems: dropItems
		acceptBlock: aBlock;
		openBlockedDialog
]

{ #category : 'actions' }
StAbstractSelectionPresenter >> accept [
	acceptBlock value: self selectedItem value: table selectedItems
]

{ #category : 'accessing' }
StAbstractSelectionPresenter >> components [
	^ super components , { dropList . dropLabel }
]

{ #category : 'initialization' }
StAbstractSelectionPresenter >> initializeDropList [

	dropList := self newDropList.
	dropList
		display: [ :scope | scope name ];
		displayIcon: [ :each | (self iconNamed: each systemIconName) ]
]

{ #category : 'initialization - deprecated' }
StAbstractSelectionPresenter >> initializeWidgets [
	super initializeWidgets .
	dropLabel := self newLabel.
	self initializeDropList
]

{ #category : 'initialization' }
StAbstractSelectionPresenter >> label: aString dropLabel: dropString withItems: items selecting: selItems dropItems: dropItems acceptBlock: aBlock [
	self label: aString withItems: items selecting: selItems.
	dropLabel label: dropString.
	dropList items: dropItems.
	acceptBlock := aBlock
]

{ #category : 'accessing' }
StAbstractSelectionPresenter >> selectedItem [
	^ dropList selectedItem
]
