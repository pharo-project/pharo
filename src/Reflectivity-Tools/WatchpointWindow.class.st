"
I am a minimal user interface that shows the last entries in the watchpoint history.
"
Class {
	#name : #WatchpointWindow,
	#superclass : #ComposablePresenter,
	#instVars : [
		'watchpoint',
		'numItems',
		'menu',
		'list',
		'inspectIt'
	],
	#category : #'Reflectivity-Tools-Watchpoints'
}

{ #category : #specs }
WatchpointWindow class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :col |
			col
				add: #menu height: (self toolbarHeight);
				add: #list;
				add: #inspectIt height: self toolbarHeight ]
]

{ #category : #'instance creation' }
WatchpointWindow class >> new [
	"use on: aWatchpoint"
	^self shouldNotImplement 
]

{ #category : #'instance creation' }
WatchpointWindow class >> on: aWatchpoint [
	| inst |
	inst := self basicNew.
	inst watchpoint: aWatchpoint.
	inst initialize.
	^inst
]

{ #category : #'instance creation' }
WatchpointWindow class >> openOn: aWatchpoint [
	^(self on: aWatchpoint) openWithSpec
]

{ #category : #accessing }
WatchpointWindow >> defaultWindowModelClass [

	^ TickingWindowPresenter
]

{ #category : #api }
WatchpointWindow >> extent [
	^230@240
]

{ #category : #initialization }
WatchpointWindow >> initializePresenter [ 
	
	list whenSelectionChanged: [inspectIt enabled: (list getIndex = 0) not ].
	
	"have to do it this way because list selectedItem is the originally selected item"
	inspectIt action: [ |index |
		index := list getIndex.
		index = 0 ifFalse: [ (list listItems at: index) inspect ]]
]

{ #category : #initialization }
WatchpointWindow >> initializeWidgets [

	list := self newList.
	inspectIt := self newButton.
	
	self setupMenu.
	
	list displayBlock: [ :assoc | assoc value asString].
	numItems := 7.
	self refreshItems.
	
	inspectIt label: 'Inspect...'.
	inspectIt enabled: false.
]

{ #category : #accessing }
WatchpointWindow >> inspectIt [
	^ inspectIt
]

{ #category : #accessing }
WatchpointWindow >> inspectIt: anObject [
	inspectIt := anObject
]

{ #category : #configuration }
WatchpointWindow >> lessItems [
	numItems := 0 max: (numItems - 1)
]

{ #category : #accessing }
WatchpointWindow >> list [
	^ list
]

{ #category : #accessing }
WatchpointWindow >> list: anObject [
	list := anObject
]

{ #category : #accessing }
WatchpointWindow >> menu [
	^ menu
]

{ #category : #accessing }
WatchpointWindow >> menu: anObject [
	menu := anObject
]

{ #category : #configuration }
WatchpointWindow >> moreItems [
	numItems := numItems + 1
]

{ #category : #updating }
WatchpointWindow >> refreshItems [
	| max values |
	values := self watchpoint values.
	max := values size.
	list items: (values copyFrom: (1 max: max - numItems) to: max) reversed. 
]

{ #category : #initialization }
WatchpointWindow >> setupMenu [
	menu := MenuPresenter new.
	menu applyTo: self.
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Show More Items';
						icon: (self iconNamed: #up);
						action: [ self moreItems ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Show Less Items';
						icon: (self iconNamed: #down);
						action: [ self lessItems ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Remove All Items';
						icon: (self iconNamed: #smallDelete);
						action: [ self watchpoint deleteHistory ] ] ].
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Stop Monitoring';
						icon: (self iconNamed: #stop);
						action: [ self watchpoint stop ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Start Monitoring';
						icon: (self iconNamed: #smallPrintIt);
						action: [ self watchpoint start ] ] ].
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Inspect ...';
						icon: (self iconNamed: #smallInspectIt);
						action: [ self watchpoint inspect ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Browse ...';
						icon: (self iconNamed: #nautilus);
						action:
								[ Smalltalk tools browser openOnMethod: self watchpoint compiledMethod ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Remove !';
						icon: (self iconNamed: #delete);
						action: [ self watchpoint uninstall ] ] ]
]

{ #category : #updating }
WatchpointWindow >> step [
	self refreshItems 
]

{ #category : #api }
WatchpointWindow >> title [
	| wpName |
	wpName := self watchpoint tagname.
	^'WP: ',(wpName isAllSeparators ifTrue: [ 'Unnamed' ] ifFalse: [ wpName ]).
]

{ #category : #accessing }
WatchpointWindow >> watchpoint [
	^ watchpoint
]

{ #category : #accessing }
WatchpointWindow >> watchpoint: anObject [
	watchpoint := anObject
]
