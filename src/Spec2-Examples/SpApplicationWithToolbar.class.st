"
I am a simple example showing how to use a menu model to have a menu toolbar.

self example
"
Class {
	#name : #SpApplicationWithToolbar,
	#superclass : #SpPresenter,
	#instVars : [
		'menu',
		'text'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #specs }
SpApplicationWithToolbar class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #menu withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		add: #text;
		yourself
]

{ #category : #example }
SpApplicationWithToolbar class >> example [
	<sampleInstance>
	^ self new openWithSpec
]

{ #category : #initialization }
SpApplicationWithToolbar >> addItemTo: aGroup [
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
SpApplicationWithToolbar >> initializePresenters [
	menu := self newMenuBar
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
						name: '';
						description: 'Open file';
						icon: (self iconNamed: #openIcon);
						action: [ self inform: 'Open File' ] ].
			group
				addItem: [ :item | 
					item
						name: '';
						description: 'Save File';
						icon: (self iconNamed: #smallSaveIcon);
						action: [ self inform: 'Save File' ] ].
			group
				addItem: [ :item | 
					item
						name: '';
						description: 'Print file';
						icon: (self iconNamed: #smallPrintIcon);
						action: [ self inform: 'Print file' ] ] ];
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: '';
						description: 'Undo';
						icon: (self iconNamed: #smallUndoIcon);
						action: [ self inform: 'Undo' ] ].
			group
				addItem: [ :item | 
					item
						name: '';
						description: 'Redo';
						icon: (self iconNamed: #smallRedoIcon);
						action: [ self inform: 'Redo' ] ] ];
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: '';
						description: 'Add menu item';
						icon: (self iconNamed: #add);
						action: [ self addItemTo: group ] ] ].
	menu applyTo: self.
	text := self newText.
	self focusOrder add: text
]

{ #category : #accessing }
SpApplicationWithToolbar >> menu [
	^ menu
]

{ #category : #initialization }
SpApplicationWithToolbar >> subMenu [
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
				name: 'Kill';
				shortcut: $k meta;
				icon: (self iconNamed: #smallCancelIcon);
				action: [ self inform: 'Kill' ] ];
		yourself
]

{ #category : #accessing }
SpApplicationWithToolbar >> text [
	^ text
]

{ #category : #api }
SpApplicationWithToolbar >> title [

	^ 'Text editor'
]
