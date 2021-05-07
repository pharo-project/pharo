Class {
	#name : #SpChooserPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'list',
		'selectedList',
		'addButton',
		'addAllButton',
		'removeButton',
		'removeAllButton',
		'buttonBar',
		'items',
		'selectedItems',
		'methodName',
		'acceptBlock'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #example }
SpChooserPresenter class >> exampleClasses [

	self openOn: 
		{(RBBrowserEnvironment default allClasses sorted: #name ascending) . 
		{ SpChooserPresenter . RefactoringSettings } .
		#name}
]

{ #category : #example }
SpChooserPresenter class >> examplePackages [

	self openOn: 
		{(RPackageOrganizer default packages sorted: #packageName ascending) . 
		{ RPackageOrganizer default packageNamed: 'Refactoring-Core' } .
		#packageName}
]

{ #category : #'instance creation' }
SpChooserPresenter class >> openOn: aCollection [
	|temp|
	temp := (self on: aCollection).
	^ temp openModalWithSpec
]

{ #category : #'instance creation' }
SpChooserPresenter class >> openOn: aCollection onAccept: aBlock [
	|temp|
	temp := (self on: aCollection).
	temp onAccept: aBlock.
	^ temp openModalWithSpec
]

{ #category : #adding }
SpChooserPresenter >> addAll [

	selectedList items: 
		((selectedList unfilteredItems, 
		list unfilteredItems) sorted: self methodName ascending).
	list items: #().
	list selectItems: #()
]

{ #category : #accessing }
SpChooserPresenter >> addAllButton [

	^ addAllButton
]

{ #category : #adding }
SpChooserPresenter >> addAllMessage [ 
	^ 'Add all items to selected list.'
]

{ #category : #accessing }
SpChooserPresenter >> addButton [

	^ addButton
]

{ #category : #adding }
SpChooserPresenter >> addSelected [

	selectedList items: 
		(selectedList items, 
		(list selectedItems sorted: self methodName ascending)).
	list items: ((list unfilteredItems \ list selectedItems) sorted: self methodName ascending).
	list applyFilter: list filterText.
	list selectItems: #()
]

{ #category : #adding }
SpChooserPresenter >> addSelectedMessage [
	^ 'Add selected items to selected list.'
]

{ #category : #adding }
SpChooserPresenter >> doAccept [
	
	acceptBlock ifNotNil: [ acceptBlock cull: self selectedList items]
]

{ #category : #adding }
SpChooserPresenter >> initalizeSelectedList [
	
	selectedList		
		display: [ :each | each perform: self methodName ]
]

{ #category : #adding }
SpChooserPresenter >> initializeButtonBar [
			
	buttonBar 
		add: self addButton 
			expand: false;
		add: self addAllButton
			expand: false;
		add: self removeButton
			expand: false;
		add: self removeAllButton
			expand: false

]

{ #category : #adding }
SpChooserPresenter >> initializeButtons [
			
	addButton := self newButton
				addStyle: 'small';
				label: '>';
				help: self addSelectedMessage;
				action: [ self addSelected ];
				yourself.
				
	addAllButton := self newButton
				addStyle: 'small';
				label: '>>';
				help: self addAllMessage;
				action: [ self addAll ];
				yourself.
				
	removeButton := self newButton
				addStyle: 'small';
				label: '<';
				help: self removeSelectedMessage;
				action: [ self removeSelected ];
				yourself.
				
	removeAllButton := self newButton
				addStyle: 'small';
				label: '<<';
				help: self removeAllMessage;
				action: [ self removeAll ];
				yourself
]

{ #category : #initialization }
SpChooserPresenter >> initializeDialogWindow: aWindowPresenter [
	
	aWindowPresenter
		addButton: 'Accept' do: [ :presenter |
			self doAccept.
			presenter
				beOk;
				close];
		addButton: 'Cancel' do: [ :presenter | 
			presenter
				beCancel;
				close ]
]

{ #category : #adding }
SpChooserPresenter >> initializeLayout [
	
	self layout: (SpBoxLayout newTopToBottom
		spacing: 5;
		add: (SpPanedLayout newLeftToRight
			add: (SpBoxLayout newLeftToRight
				spacing: 5;
				add: (list := (self instantiate: SpFilteringSelectableListPresenter));
				add: (buttonBar := SpBoxLayout newTopToBottom) expand: false;
				yourself);
			add: (selectedList := (self instantiate: SpFilteringSelectableListPresenter));
			yourself);
		yourself)
]

{ #category : #adding }
SpChooserPresenter >> initializeList [
	
	list
		display: [ :each | each perform: self methodName ]
]

{ #category : #adding }
SpChooserPresenter >> initializePresenters [
	<script: 'self new openWithSpec'>

	self initializeLayout.
	self initializeList.
	self initalizeSelectedList.
	self initializeButtons.
	self initializeButtonBar
]

{ #category : #adding }
SpChooserPresenter >> initializeWindow: aWindowPresenter [

	aWindowPresenter 
		title: self title;
		initialExtent: 700@500
]

{ #category : #adding }
SpChooserPresenter >> items [

	^ items ifNil: [ items := {  } ]
]

{ #category : #accessing }
SpChooserPresenter >> list [
	^ list
]

{ #category : #adding }
SpChooserPresenter >> methodName [ 
	^ methodName
]

{ #category : #events }
SpChooserPresenter >> onAccept: aBlock [

	acceptBlock := aBlock
]

{ #category : #adding }
SpChooserPresenter >> removeAll [

	selectedList items: #().
	list items: self items
]

{ #category : #accessing }
SpChooserPresenter >> removeAllButton [

	^ removeAllButton
]

{ #category : #adding }
SpChooserPresenter >> removeAllMessage [
	^ 'Remove all items from selected list.'
]

{ #category : #accessing }
SpChooserPresenter >> removeButton [

	^ removeButton
]

{ #category : #adding }
SpChooserPresenter >> removeSelected [

	list items: ((list unfilteredItems, selectedList selectedItems) sorted: self methodName ascending).
	list applyFilter: list filterText.
	selectedList items: ((selectedList unfilteredItems \ selectedList selectedItems) sorted: self methodName ascending).
	selectedList applyFilter: selectedList filterText.
	selectedList selectItems: #()
]

{ #category : #adding }
SpChooserPresenter >> removeSelectedMessage [
	^'Remove selected items from selected list.'

]

{ #category : #adding }
SpChooserPresenter >> selectedItems [

	^ selectedItems ifNil: [ selectedItems := {  } ]
]

{ #category : #accessing }
SpChooserPresenter >> selectedList [
	^ selectedList
]

{ #category : #initialization }
SpChooserPresenter >> setModelBeforeInitialization: aCollection [
	items := aCollection first.
	selectedItems := aCollection second.
	methodName :=  aCollection third.
]

{ #category : #adding }
SpChooserPresenter >> title [ 
	^ 'Choose items'
]

{ #category : #adding }
SpChooserPresenter >> updatePresenter [

	list items: self items \ self selectedItems.
	selectedList items: self selectedItems
]
