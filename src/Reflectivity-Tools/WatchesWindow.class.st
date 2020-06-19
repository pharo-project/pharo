"
I am a minimal user interface that shows the last entries in the watchpoint history.
"
Class {
	#name : #WatchesWindow,
	#superclass : #ComposablePresenter,
	#instVars : [
		'numItems',
		'menu',
		'list',
		'inspectIt',
		'watch'
	],
	#category : #'Reflectivity-Tools-Watches'
}

{ #category : #specs }
WatchesWindow class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :col |
			col
				add: #menu height: (self toolbarHeight);
				add: #list;
				add: #inspectIt height: self toolbarHeight ]
]

{ #category : #'instance creation' }
WatchesWindow class >> new [
	"use on: aWatchpoint"
	^self shouldNotImplement 
]

{ #category : #'instance creation' }
WatchesWindow class >> on: aWatchpoint [
	| inst |
	inst := self basicNew.
	inst watch: aWatchpoint.
	inst initialize.
	^inst
]

{ #category : #'instance creation' }
WatchesWindow class >> openOn: aWatchpoint [
	^(self on: aWatchpoint) openWithSpec
]

{ #category : #accessing }
WatchesWindow >> defaultWindowModelClass [

	^ TickingWindowPresenter
]

{ #category : #api }
WatchesWindow >> extent [
	^230@240
]

{ #category : #initialization }
WatchesWindow >> initializePresenter [ 
	
	list whenSelectionChanged: [inspectIt enabled: (list getIndex = 0) not ].
	
	"have to do it this way because list selectedItem is the originally selected item"
	inspectIt action: [ |index |
		index := list getIndex.
		index = 0 ifFalse: [ (list listItems at: index) inspect ]]
]

{ #category : #initialization }
WatchesWindow >> initializeWidgets [

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
WatchesWindow >> inspectIt [
	^ inspectIt
]

{ #category : #accessing }
WatchesWindow >> inspectIt: anObject [
	inspectIt := anObject
]

{ #category : #configuration }
WatchesWindow >> lessItems [
	numItems := 0 max: (numItems - 1)
]

{ #category : #accessing }
WatchesWindow >> list [
	^ list
]

{ #category : #accessing }
WatchesWindow >> list: anObject [
	list := anObject
]

{ #category : #accessing }
WatchesWindow >> menu [
	^ menu
]

{ #category : #accessing }
WatchesWindow >> menu: anObject [
	menu := anObject
]

{ #category : #configuration }
WatchesWindow >> moreItems [
	numItems := numItems + 1
]

{ #category : #updating }
WatchesWindow >> refreshItems [
	| max values |
	values := self watch values.
	max := values size.
	list items: (values copyFrom: (1 max: max - numItems) to: max) reversed. 
]

{ #category : #initialization }
WatchesWindow >> setupMenu [
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
						action: [ self watch deleteHistory ] ] ].
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Stop Monitoring';
						icon: (self iconNamed: #stop);
						action: [ self watch stop ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Start Monitoring';
						icon: (self iconNamed: #smallPrintIt);
						action: [ self watch start ] ] ].
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Inspect ...';
						icon: (self iconNamed: #smallInspectIt);
						action: [ self watch inspect ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Browse ...';
						icon: (self iconNamed: #nautilus);
						action:
								[ Smalltalk tools browser openOnMethod: self watch compiledMethod ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Remove !';
						icon: (self iconNamed: #delete);
						action: [ self watch uninstall ] ] ]
]

{ #category : #updating }
WatchesWindow >> step [
	self refreshItems 
]

{ #category : #api }
WatchesWindow >> title [
	| wpName |
	wpName := self watch tagname.
	^'WP: ',(wpName isAllSeparators ifTrue: [ 'Unnamed' ] ifFalse: [ wpName ]).
]

{ #category : #accessing }
WatchesWindow >> watch [
	^ watch
]

{ #category : #accessing }
WatchesWindow >> watch: anObject [
	watch := anObject
]
