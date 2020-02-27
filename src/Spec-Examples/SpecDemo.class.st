"
Set of demos that show capabilities of the Spec UI framework.

SpecDemo open
"
Class {
	#name : #SpecDemo,
	#superclass : #ComposablePresenter,
	#instVars : [
		'menu',
		'list',
		'page',
		'selectedPage'
	],
	#category : #'Spec-Examples-Demo'
}

{ #category : #specs }
SpecDemo class >> defaultSpec [

	<spec: #default>

	| fontWidth listWidth |
	fontWidth := (StandardFonts defaultFont widthOfString: 'M').
	listWidth := fontWidth * 20.
	
	^ SpecLayout composed
		newColumn: [ :c | 
			c add: #menu height: self toolbarHeight.
			c newRow: [ :row | 
				row add: #list right: listWidth.
				row addSplitter.
				row add: #page] ];
		yourself
	
	
]

{ #category : #'user interface' }
SpecDemo class >> menuExamplesOn: aBuilder [
	"Add a custom menu item to the world menu"

	<worldMenu>
	(aBuilder item: #'Spec examples')
		iconName: #smallHelpIcon;
		parent: #Help;
		order: 3.5;
		help: 'Open Spec demo browser.';
		action: [ self open ].
	aBuilder withSeparatorAfter	
]

{ #category : #'instance creation' }
SpecDemo class >> on: aModel [

	^ self basicNew 
		model: aModel;
		initialize;
		yourself.
]

{ #category : #specs }
SpecDemo class >> open [

	<example>
	
	^ self new
		openWithSpec.

]

{ #category : #accessing }
SpecDemo >> addItemTo: aGroup [
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
SpecDemo >> availablePages [

	^ SpecDemoPage allSubclasses sort: [ :a :b | a priority <= b priority ]
]

{ #category : #accessing }
SpecDemo >> defaultPage [

	^ self availablePages first.
]

{ #category : #api }
SpecDemo >> extent [

	| fontWidth rowHeight |
	fontWidth := (StandardFonts defaultFont widthOfString: 'M').
	rowHeight := self class inputTextHeight.

	^ (90@24) * (fontWidth @ rowHeight)
	
]

{ #category : #initialization }
SpecDemo >> initializePresenter [

	list setSelectedItem: self selectedPage.
	
	list whenSelectedItemChanged: [ :newPageClass |
		newPageClass
			ifNotNil: [
			newPageClass = self selectedPage
				ifFalse: [
					self selectedPage: newPageClass. 
					page := self instantiate: newPageClass.
				
					self needRebuild: false.
					self buildWithSpec.
					list setSelectedItem: self selectedPage ] ]
			ifNil: [ "Avoid the user to have no page selected at all."
				list setSelectedItem: self selectedPage ] ].
]

{ #category : #initialization }
SpecDemo >> initializeWidgets [

	menu := self mainMenu.
	menu applyTo: self.
	
	selectedPage := self availablePages first.
	
	list := self newList.
	list 
		items: self availablePages;
		displayBlock: [:item | item pageName].

	page := (self instantiate: selectedPage).
	
	self focusOrder 
		add: list;
		add: page.
		
]

{ #category : #accessing }
SpecDemo >> list [
	^ list
]

{ #category : #accessing }
SpecDemo >> list: anObject [
	list := anObject
]

{ #category : #initialization }
SpecDemo >> mainMenu [

	| aMenu |
	aMenu := MenuPresenter new
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
						icon: (self iconNamed:  #add);
						action: [ self addItemTo: group ] ] ].
	^ aMenu.

]

{ #category : #accessing }
SpecDemo >> menu [
	^ menu
]

{ #category : #accessing }
SpecDemo >> menu: anObject [
	menu := anObject
]

{ #category : #accessing }
SpecDemo >> page [
	^ page
]

{ #category : #accessing }
SpecDemo >> page: anObject [
	page := anObject
]

{ #category : #accessing }
SpecDemo >> selectedPage [
	^ selectedPage
]

{ #category : #accessing }
SpecDemo >> selectedPage: anObject [
	selectedPage := anObject
]

{ #category : #initialization }
SpecDemo >> subMenu [ 

	^ MenuPresenter new
		addGroup: [ :group |
			group addItem: [ :item |
				item
					name: 'Open';
					icon: (self iconNamed: #openIcon);
					shortcut: $o meta;
					action: [ self inform: 'Open' ] ].
			group addItem: [ :item |
				item 
					name: 'Save';
					icon: (self iconNamed: #smallSaveIcon);
					shortcut: $s meta;
					action: [ self inform: 'Save' ] ].
			group addItem: [ :item |
				item 
					name: 'Print';
					shortcut: $p meta;
					icon: (self iconNamed: #smallPrintIcon);
					action: [ self inform: 'Print' ] ].
			group addItem: [ :item |
				item 
					name: 'Close';
					shortcut: $c meta;
					icon: (self iconNamed: #smallCancelIcon);
					action: [ self inform: 'Kill' ] ] ]
]

{ #category : #api }
SpecDemo >> title [
	
	^ 'Spec UI Framework Demo'.
]
