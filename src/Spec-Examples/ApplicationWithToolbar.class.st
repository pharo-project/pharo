"
I am a simple example showing how to use a menu model to have a menu toolbar.

self example
"
Class {
	#name : #ApplicationWithToolbar,
	#superclass : #ComposablePresenter,
	#instVars : [
		'menu',
		'text'
	],
	#category : #Spec-Examples
}

{ #category : #specs }
ApplicationWithToolbar class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :c | 
			c 
				add: #menu height: self toolbarHeight;
				add: #text ];
		yourself
]

{ #category : #example }
ApplicationWithToolbar class >> example [
	^ self new openWithSpec
]

{ #category : #initialization }
ApplicationWithToolbar >> addItemTo: aGroup [
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

{ #category : #initialization }
ApplicationWithToolbar >> initializeWidgets [
	menu := MenuPresenter new
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
	menu applyTo: self.
	text := self newText.
	self focusOrder add: text
]

{ #category : #accessing }
ApplicationWithToolbar >> menu [
	^ menu
]

{ #category : #initialization }
ApplicationWithToolbar >> subMenu [ 

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
					name: 'Kill';
					shortcut: $k meta;
					icon: (self iconNamed: #smallCancelIcon);
					action: [ self inform: 'Kill' ] ] ]
]

{ #category : #accessing }
ApplicationWithToolbar >> text [
	^ text
]

{ #category : #api }
ApplicationWithToolbar >> title [

	^ 'Text editor'
]
