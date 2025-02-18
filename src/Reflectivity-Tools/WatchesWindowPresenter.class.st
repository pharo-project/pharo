"
I present the values captured in a Watch.
To open me, click on the left icon next to a watch in the code browser.
"
Class {
	#name : 'WatchesWindowPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'numItems',
		'menu',
		'list',
		'inspectIt',
		'watch',
		'service'
	],
	#category : 'Reflectivity-Tools-Watches',
	#package : 'Reflectivity-Tools',
	#tag : 'Watches'
}

{ #category : 'layout' }
WatchesWindowPresenter class >> defaultLayout [

	^ SpBoxLayout newTopToBottom
			add: #menu expand: false;
			add: #list;
			add: #inspectIt expand: false;
			yourself
]

{ #category : 'accessing' }
WatchesWindowPresenter class >> defaultPreferredExtent [

	^ 270@240
]

{ #category : 'instance creation' }
WatchesWindowPresenter class >> on: aWatchpoint [
	| inst |
	inst := self basicNew.
	inst watch: aWatchpoint.
	inst initialize.
	^inst
]

{ #category : 'instance creation' }
WatchesWindowPresenter class >> openOn: aWatchpoint [
	^(self on: aWatchpoint) open
]

{ #category : 'initialization' }
WatchesWindowPresenter >> connectPresenters [

	list whenSelectionChangedDo: [ :selection |
		inspectIt enabled: selection isEmpty not ].

	inspectIt action: [ list selectedItem inspect ]
]

{ #category : 'initialization' }
WatchesWindowPresenter >> initializePresenters [

	list := self newList.
	inspectIt := self newButton.

	self setupMenu.

	list display: [ :assoc | assoc value asString].
	numItems := 7.
	self refreshItems.

	inspectIt label: 'Inspect...'.
	inspectIt enabled: false
]

{ #category : 'initialization' }
WatchesWindowPresenter >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter. 
	aWindowPresenter whenOpenedDo: [ self startService ].
	aWindowPresenter whenClosedDo: [ self stopService ].
]

{ #category : 'accessing' }
WatchesWindowPresenter >> inspectIt [
	^ inspectIt
]

{ #category : 'accessing' }
WatchesWindowPresenter >> inspectIt: anObject [
	inspectIt := anObject
]

{ #category : 'configuration' }
WatchesWindowPresenter >> lessItems [
	numItems := 0 max: (numItems - 1)
]

{ #category : 'accessing' }
WatchesWindowPresenter >> list [
	^ list
]

{ #category : 'accessing' }
WatchesWindowPresenter >> list: anObject [
	list := anObject
]

{ #category : 'accessing' }
WatchesWindowPresenter >> menu [
	^ menu
]

{ #category : 'accessing' }
WatchesWindowPresenter >> menu: anObject [
	menu := anObject
]

{ #category : 'configuration' }
WatchesWindowPresenter >> moreItems [
	numItems := numItems + 1
]

{ #category : 'updating' }
WatchesWindowPresenter >> refreshItems [
	| max values |
	values := self watch values.
	max := values size.
	list items: (values copyFrom: (1 max: max - numItems) to: max) reversed.
]

{ #category : 'initialization' }
WatchesWindowPresenter >> setupMenu [

	menu := self newToolbar.
	menu beIcons.

	menu
		addItem: (self newToolbarButton
						label: '';
						help: 'Show More Items';
						icon: (self iconNamed: #up);
						action: [ self moreItems ];
						yourself).

	menu
		addItem: (self newToolbarButton
						label: nil;
						help: 'Show Less Items';
						icon: (self iconNamed: #down);
						action: [ self lessItems ];
						yourself).

	menu
		addItem: (self newToolbarButton
						label: nil;
						help: 'Remove All Items';
						icon: (self iconNamed: #smallDelete);
						action: [ self watch deleteHistory ];
						yourself).

	menu
		addItem: (self newToolbarButton
						label: nil;
						help: 'Stop Monitoring';
						icon: (self iconNamed: #stop);
						action: [ self watch stop ];
						yourself).

	menu
		addItem: (self newToolbarButton
						label: nil;
						help: 'Start Monitoring';
						icon: (self iconNamed: #smallPrintIt);
						action: [ self watch start ];
						yourself).

	menu
		addItem: (self newToolbarButton
						label: nil;
						help: 'Inspect ...';
						icon: (self iconNamed: #smallInspectIt);
						action: [ self watch inspect ];
						yourself).

	menu
		addItem: (self newToolbarButton
						label: nil;
						help: 'Browse ...';
						icon: (self iconNamed: #nautilus);
						action: [ (self application toolNamed: #browser) openOnMethod: self watch compiledMethod ];
						yourself).

	menu
		addItem: (self newToolbarButton
						label: nil;
						help: 'Remove !';
						icon: (self iconNamed: #delete);
						action: [ self watch uninstall ];
						yourself)
]

{ #category : 'updating' }
WatchesWindowPresenter >> startService [

	service := TKTParameterizableService new.
	service name: 'WatchesWindowPresenter: ' , self hash asString.
	service step: [ self refreshItems ].
	service start
]

{ #category : 'updating' }
WatchesWindowPresenter >> stopService [

	service stop
]

{ #category : 'api' }
WatchesWindowPresenter >> title [
	| wpName |
	wpName := self watch tagname.
	^'WP: ',(wpName isAllSeparators ifTrue: [ 'Unnamed' ] ifFalse: [ wpName ])
]

{ #category : 'accessing' }
WatchesWindowPresenter >> watch [
	^ watch
]

{ #category : 'accessing' }
WatchesWindowPresenter >> watch: anObject [
	watch := anObject
]
