"
A KMDescription is a window showing the description of shortcuts for a specified set of symbols reprensenting KMCategories
"
Class {
	#name : 'KMDescriptionPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'shortcutList',
		'actionBar',
		'categoryList',
		'shortcutDetails',
		'selectedAction'
	],
	#category : 'Keymapping-Tools-Spec',
	#package : 'Keymapping-Tools-Spec'
}

{ #category : 'instance creation' }
KMDescriptionPresenter class >> descriptionText [

	^ 'Show the description of shortcuts for the system'
]

{ #category : 'examples' }
KMDescriptionPresenter class >> example [
	self new
		kmCategories: #(WindowShortcuts);
		open
]

{ #category : 'accessing' }
KMDescriptionPresenter class >> icon [

	^ self iconNamed: #keymapBrowser
]

{ #category : 'instance creation' }
KMDescriptionPresenter class >> menuCommandOn: aBuilder [
	<worldMenu>

	(aBuilder item: 'Shortcuts Editor')
		action: [ self open ];
		order: 34;
		parent: #Tools;
		icon: self icon;
		help: self descriptionText
]

{ #category : 'instance creation' }
KMDescriptionPresenter class >> open [
	"Open the receiver's on all the system keymap categories"

	^ self new
		addShortcutActivationCmdInstances;
		addKeymapCategoryInstances;
		addAllAPIsEntry;
		open
]

{ #category : 'api' }
KMDescriptionPresenter >> addAllAPIsEntry [
	"Set the receiver's category items to a list of categories in aCollectionOfSymbols.
	We also build an 'All' category including all keymaps in aCollectionOfSymbols"

	| newCategoryItem kmNamedCategories |
	
	newCategoryItem := KMCategoryItemPresenter new categoryName: 'All'.
	kmNamedCategories := KMRepository default categories.
	kmNamedCategories inject: newCategoryItem into: [ : a : b | a , b ].
	newCategoryItem addAll: self shortcutActivationCmdInstances.

	categoryList prependCollection: {newCategoryItem}
]

{ #category : 'api - keymaps' }
KMDescriptionPresenter >> addKeymapCategoryInstances [

	self kmCategories: KMRepository default categories keys
]

{ #category : 'api - commander' }
KMDescriptionPresenter >> addShortcutActivationCmdInstances [

	categoryList appendCollection: self shortcutActivationCategoriesItems.
]

{ #category : 'initialization' }
KMDescriptionPresenter >> connectPresenters [

	categoryList
		transmitTo: shortcutList
		transform: [ :item | self keymapsAtCategory: item ].

	shortcutList
		transmitTo: shortcutDetails
		transform: [ :item | self shortcutDetailsFor: item ]
		postTransmission: [ :presenter | selectedAction ifNotNil: [ :actionBlock | self updateCodePresenter: actionBlock ] ]
]

{ #category : 'layout' }
KMDescriptionPresenter >> defaultLayout [

	^ SpPanedLayout newTopToBottom
		  add: (SpBoxLayout newTopToBottom
				   spacing: 5;
				   add: (SpBoxLayout newLeftToRight
						    add: 'Filter scopes' expand: false;
						    add: categoryList;
						    yourself)
				   expand: false;
				   add: shortcutList);
		  add: (SpBoxLayout newTopToBottom
				   add: shortcutDetails;
				   add: actionBar withConstraints: [ :c | c height: 25 ]);
		  yourself
]

{ #category : 'callbacks' }
KMDescriptionPresenter >> highlightColor [

	^ self application configuration isDarkTheme 
		ifTrue: [ Color lightGray ]
		ifFalse: [ Color veryVeryLightGray ]
]

{ #category : 'initialization' }
KMDescriptionPresenter >> initializeActionBar [

	actionBar := self newActionBar.
	actionBar
		addLast:
			(SpButtonPresenter new
				action: [ self window close ];
				label: 'Close';
				yourself)
]

{ #category : 'initialization' }
KMDescriptionPresenter >> initializeCategoryList [

	categoryList := self newDropList.
	categoryList display: [ :category | category name ].
]

{ #category : 'initialization' }
KMDescriptionPresenter >> initializePresenters [

	self
		initializeCategoryList; 
		initializeShortcutList;
		initializeShortcutDetails;
		initializeActionBar
]

{ #category : 'initialization' }
KMDescriptionPresenter >> initializeShortcutDetails [

	shortcutDetails := self newCode.

]

{ #category : 'initialization' }
KMDescriptionPresenter >> initializeShortcutList [

	shortcutList := self newTable.
	shortcutList
		addColumn: (SpStringTableColumn new
			title: 'API';
			evaluated: [ : each | each class name ];
			beSortable;
			yourself);
		addColumn: (SpStringTableColumn new  
			title: 'Scope'; 
			evaluated: [ :each | each scopeName ]; 
			beSortable;
			yourself);
		addColumn: (SpStringTableColumn title: 'Shortcut' evaluated: #shortcut);
		addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
		addColumn: (SpStringTableColumn title: 'Description' evaluated: #description);
		beResizable.
]

{ #category : 'initialization' }
KMDescriptionPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: 'Shortcuts Editor';
		initialExtent: 900 @ 650
]

{ #category : 'initialization' }
KMDescriptionPresenter >> keymapsAtCategory: aKMCategoryOrKMCategoryItemPresenter [
	"Answer a <Collection> of keymaps "

	^ (aKMCategoryOrKMCategoryItemPresenter isKindOf: KMCategoryItemPresenter) 
		ifFalse: [ (aKMCategoryOrKMCategoryItemPresenter model entriesAt: #all) keymaps asOrderedCollection ]
		ifTrue: [ aKMCategoryOrKMCategoryItemPresenter shortcuts ]
]

{ #category : 'api - keymaps' }
KMDescriptionPresenter >> kmCategories: aCollectionOfSymbols [
	"Set the receiver's category items to a list of categories in aCollectionOfSymbols.
	We also build an 'All' category including all keymaps in aCollectionOfSymbols"

	categoryList appendCollection: (self kmCategoriesAt: aCollectionOfSymbols)
]

{ #category : 'api - keymaps' }
KMDescriptionPresenter >> kmCategoriesAt: aCollectionOfSymbols [

	^ aCollectionOfSymbols 
		collect: [ :e | KMRepository default categoryForName: e ]
		as: OrderedCollection
]

{ #category : 'api - commander' }
KMDescriptionPresenter >> shortcutActivationCategories [
	"Answer a <Collection> of <...> representing each a category of shortcuts created using the Commander 1 framework"

	^ (self shortcutActivationCmdInstances 
		groupedBy: [ : cmdShortcutActivation | cmdShortcutActivation annotatedClass packageName ]) values
]

{ #category : 'api - commander' }
KMDescriptionPresenter >> shortcutActivationCategoriesItems [
	"Answer a <Collection> of <KMCategoryItemPresenter> representing each a category of shortcuts created using the Commander 1 framework"

	^ self shortcutActivationCategories
			collect: [ : shortcuts | 
				KMCategoryItemPresenter new 
					categoryName: shortcuts anyOne annotatedClass packageName;
					shortcuts: shortcuts;
					yourself ]
]

{ #category : 'api - commander' }
KMDescriptionPresenter >> shortcutActivationCmdInstances [

	^ CmdShortcutActivation registeredInstances copyWithoutAll: CmdShortcutActivation redefiningInstances
]

{ #category : 'callbacks' }
KMDescriptionPresenter >> shortcutDetailsFor: anObject [ 

	^ anObject 
		ifNotNil: [ : aKMKeymapOrCmdShortcutActivation | 
			selectedAction := aKMKeymapOrCmdShortcutActivation action.
			selectedAction method sourceCode ]
		ifNil: [ String empty ]
]

{ #category : 'callbacks' }
KMDescriptionPresenter >> updateCodePresenter: actionBlock [

	shortcutDetails
		beForMethod: selectedAction method;
		text: selectedAction method sourceCode.

	selectedAction isClosure ifTrue: [
		shortcutDetails addTextSegmentDecoration:
			(SpTextPresenterDecorator forHighlight
				 interval: (selectedAction sourceNode sourceInterval first to:
						  selectedAction sourceNode sourceInterval last + 1);
				 highlightColor: self highlightColor;
				 yourself) ]
]
