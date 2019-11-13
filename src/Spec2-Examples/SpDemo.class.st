"
Set of demos that show capabilities of the Spec UI framework.

SpecDemo open
"
Class {
	#name : #SpDemo,
	#superclass : #SpPresenter,
	#instVars : [
		'menu',
		'list',
		'page',
		'selectedPage'
	],
	#category : #'Spec2-Examples-Demo'
}

{ #category : #specs }
SpDemo class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #menu withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		add:
			(SpPanedLayout newHorizontal
				position: 200;
				add: #list;
				add: #page;
				yourself);
		yourself
]

{ #category : #'user interface' }
SpDemo class >> menuExamplesOn: aBuilder [
	"Add a custom menu item to the world menu"

	<worldMenu>
	(aBuilder item: #'Spec2 examples')
		iconName: #smallHelpIcon;
		parent: #Help;
		order: 3.5;
		help: 'Open Spec2 demo browser.';
		action: [ self open ].
	aBuilder withSeparatorAfter	
]

{ #category : #'instance creation' }
SpDemo class >> on: aModel [

	^ self basicNew 
		model: aModel;
		initialize;
		yourself.
]

{ #category : #specs }
SpDemo class >> open [
	<example>
	^ self new openWithSpec
]

{ #category : #accessing }
SpDemo >> addItemTo: aGroup [
	aGroup
		addItem: [ :item | 
			item
				name: 'Dynamic';
				icon: (self iconNamed: #delete);
				action: [ aGroup menuItems remove: item.
					self needRebuild: false.
					self buildWithSpec ] ].
	self needRebuild: false.
	self buildWithSpec
]

{ #category : #accessing }
SpDemo >> availablePages [
	^ SpDemoPage availablePages
]

{ #category : #initialization }
SpDemo >> connectPresenters [
	list selectItem: self selectedPage.
	list
		whenSelectionChangedDo: [ :selection | 
			selection selectedItem
				ifNotNil: [ :newPageClass | 
					newPageClass = self selectedPage
						ifFalse: [ self selectedPage: newPageClass.
							page := self instantiate: newPageClass.
							self needRebuild: false.
							self buildWithSpec.
							list selectItem: self selectedPage ] ]
				ifNil: [ "Avoid the user to have no page selected at all." list setSelectedItem: self selectedPage ] ]
]

{ #category : #accessing }
SpDemo >> defaultPage [

	^ self availablePages first.
]

{ #category : #api }
SpDemo >> initialExtent [

	| fontWidth rowHeight |
	fontWidth := (StandardFonts defaultFont widthOfString: 'M').
	rowHeight := self class inputTextHeight.

	^ (90@24) * (fontWidth @ rowHeight)
	
]

{ #category : #initialization }
SpDemo >> initializePresenters [
	menu := self mainMenu.
	list := self newList.
	page := self instantiate: (selectedPage := self availablePages first).

	menu applyTo: self.

	list
		items: self availablePages;
		display: [ :item | item pageName ];
		contextMenu:
			(self newMenu
				addItem: [ :item | 
					item
						name: 'Browse';
						icon: (self iconNamed: #smallHelp);
						action: [ list selectedItem browse ] ];
				addItem: [ :item | 
					item
						name: 'Browse presenter';
						icon: (self iconNamed: #smallHelp);
						action: [ list selectedItem new pageClass browse ] ];
				yourself).

	self focusOrder
		add: list;
		add: page
]

{ #category : #accessing }
SpDemo >> list [
	^ list
]

{ #category : #accessing }
SpDemo >> list: anObject [
	list := anObject
]

{ #category : #initialization }
SpDemo >> mainMenu [
	| aMenu |
	aMenu := self newMenuBar
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: 'File';
						icon: (self iconNamed: #openIcon);
						subMenu: self subMenu ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Open file';
						icon: (self iconNamed: #openIcon);
						action: [ self inform: 'Open File' ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Save File';
						icon: (self iconNamed: #smallSaveIcon);
						action: [ self inform: 'Save File' ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Print file';
						icon: (self iconNamed: #smallPrintIcon);
						action: [ self inform: 'Print file' ] ] ];
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Undo';
						icon: (self iconNamed: #smallUndoIcon);
						action: [ self inform: 'Undo' ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Redo';
						icon: (self iconNamed: #smallRedoIcon);
						action: [ self inform: 'Redo' ] ] ];
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Add menu item';
						icon: (self iconNamed: #add);
						action: [ self addItemTo: group ] ] ].
	^ aMenu
]

{ #category : #accessing }
SpDemo >> menu [
	^ menu
]

{ #category : #accessing }
SpDemo >> menu: anObject [
	menu := anObject
]

{ #category : #accessing }
SpDemo >> page [
	^ page
]

{ #category : #accessing }
SpDemo >> page: anObject [
	page := anObject
]

{ #category : #accessing }
SpDemo >> selectedPage [
	^ selectedPage
]

{ #category : #accessing }
SpDemo >> selectedPage: anObject [
	selectedPage := anObject
]

{ #category : #initialization }
SpDemo >> subMenu [
	^ self newMenu
		addItem: [ :item | 
			item
				name: 'Open';
				icon: (self iconNamed: #openIcon);
				shortcut: $o meta;
				action: [ self inform: 'Open' ] ];
		addItem: [ :item | 
			item
				name: 'Save';
				icon: (self iconNamed: #smallSaveIcon);
				shortcut: $s meta;
				action: [ self inform: 'Save' ] ];
		addItem: [ :item | 
			item
				name: 'Print';
				shortcut: $p meta;
				icon: (self iconNamed: #smallPrintIcon);
				action: [ self inform: 'Print' ] ];
		addItem: [ :item | 
			item
				name: 'Close';
				shortcut: $c meta;
				icon: (self iconNamed: #smallCancelIcon);
				action: [ self inform: 'Kill' ] ];
		yourself
]

{ #category : #api }
SpDemo >> title [
	
	^ 'Spec UI Framework Demo'.
]
