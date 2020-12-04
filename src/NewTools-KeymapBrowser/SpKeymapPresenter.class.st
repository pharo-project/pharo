"
A `SpKeymapPresenter`  shows a list of all defined keymappings.
It shows a filterable list of keymap shortcuts,  name and categories.
Use the context menu to browse the method that defines the shortcut.
"
Class {
	#name : #SpKeymapPresenter,
	#superclass : #StPresenter,
	#instVars : [
		'clearFilterButton',
		'filterField',
		'keymapTable'
	],
	#category : #'NewTools-KeymapBrowser'
}

{ #category : #specs }
SpKeymapPresenter class >> defaultSpec [
	<spec>

	^ SpBoxLayout newVertical
		add: (SpBoxLayout newHorizontal
				add: #filterField;
				add: #clearFilterButton expand: false;
				yourself)
			expand: false;
		add: #keymapTable;
		yourself
]

{ #category : #'system menu' }
SpKeymapPresenter class >> keymapBrowserMenuOn: aBuilder [ 
	"Build an entry in system world menu"
	<worldMenu>
	
	(aBuilder item: #'Keymap Browser')
		parent: #System;
		order: 4;
		icon: self taskbarIcon;
		action: [ self new openWithSpec ];
		help: 'List all know shortcuts and Keymapping categories.'
]

{ #category : #opening }
SpKeymapPresenter class >> open [
	<script>
	
	self new openWithSpec
]

{ #category : #accessing }
SpKeymapPresenter class >> taskbarIconName [

	^ #keymapBrowserIcon
]

{ #category : #private }
SpKeymapPresenter >> browseDefinitionOfSelectedShortcut [
	| selection action class selector |
	selection := self keymapTable selection selectedItem ifNil: [ ^ self ].
	action := selection realValue keymap action ifNil: [ ^ self ].
	class := action method methodClass.
	selector := action method selector.
	Smalltalk tools browser
		openOnClass: class
		selector: selector
		highlight: selection shortcutName
]

{ #category : #accessing }
SpKeymapPresenter >> clearFilterButton [

	^ clearFilterButton
]

{ #category : #accessing }
SpKeymapPresenter >> clearFilterButton: anObject [

	clearFilterButton := anObject
]

{ #category : #private }
SpKeymapPresenter >> collectAllShortcuts [

	self keymapTable items: self shortcuts
]

{ #category : #'private - initialization' }
SpKeymapPresenter >> connectPresenters [

	filterField
		whenTextChangedDo: [ :text | 
			clearFilterButton enabled: text isNotEmpty.
			self setFilter: text ].
	clearFilterButton action: [ filterField text: '' ]
]

{ #category : #private }
SpKeymapPresenter >> contextMenu [
	| hasSelection |
	
	hasSelection := self keymapTable selection selectedItem notNil.
	^ SpMenuPresenter new 
		addGroup: [ :group | group 
			addItem: [ :item | item 
				name: 'Refresh'; 
				icon: (self iconNamed: #smallUpdateIcon); 
				action: [ self collectAllShortcuts ] ] ];
		addGroup: [ :group | group 
			addItem: [ :item | item 
				name: 'Browse Definition';
				icon: (self iconNamed: #smallSystemBrowserIcon);
				enabled: hasSelection;
				action: [ self browseDefinitionOfSelectedShortcut ] ];
			addItem: [ :item | item 
				name: 'Inspect Action';
				icon: (self iconNamed: #smallInspectItIcon);
				enabled: hasSelection;
				action: [ self inspectActionOfSelectedShortcut ] ] ].

]

{ #category : #accessing }
SpKeymapPresenter >> filterField [

	^ filterField
]

{ #category : #accessing }
SpKeymapPresenter >> filterField: anObject [

	filterField := anObject
]

{ #category : #'private - initialization' }
SpKeymapPresenter >> initFilter [

	filterField := self newTextInput.
	filterField
		removeEntryCompletion;
		placeholder: 'Filter';
		autoAccept: true.
	
	"Add a button to be able to quickly clear the filter"	
	clearFilterButton := self newButton.
	clearFilterButton
		icon: (self iconNamed: #smallCancelIcon);
		enabled: false
]

{ #category : #'private - initialization' }
SpKeymapPresenter >> initKeyMapTable [

	keymapTable := self newTable.
	keymapTable
		addColumn: (SpStringTableColumn title: 'Shortcut' evaluated: [ :aKMShortcutDeclaration | aKMShortcutDeclaration realValue shortcut asString ]);
		addColumn: (SpStringTableColumn title: 'Name' evaluated: #shortcutName);
		addColumn: (SpStringTableColumn title: 'Category' evaluated: #categoryName);
		beResizable;
		contextMenu: [ self contextMenu ]
]

{ #category : #api }
SpKeymapPresenter >> initialExtent [

	^ 600 @ 300
]

{ #category : #initialization }
SpKeymapPresenter >> initializePresenters [

	self 
		initFilter;
		initKeyMapTable.
	
	self collectAllShortcuts
]

{ #category : #private }
SpKeymapPresenter >> inspectActionOfSelectedShortcut [
	| selection action |
	selection := self keymapTable selection selectedItem ifNil: [ ^ self ].
	action := selection realValue keymap ifNil: [ ^ self ].
	action inspect
]

{ #category : #accessing }
SpKeymapPresenter >> keymapTable [

	^ keymapTable
]

{ #category : #accessing }
SpKeymapPresenter >> keymapTable: anObject [

	keymapTable := anObject
]

{ #category : #private }
SpKeymapPresenter >> menu: aMenu shifted: aBoolean [
	(aMenu add: 'Refresh' target: self selector: #collectAllShortcuts)
		icon: (self iconNamed: #smallUpdateIcon).
	self keymapTable selection selectedItem ifNil: [ ^ aMenu ].
	(aMenu
		add: 'Browse Definition'
		target: self
		selector: #browseDefinitionOfSelectedShortcut)
		icon: (self iconNamed: #smallSystemBrowserIcon).
	(aMenu
		add: 'Inspect Action'
		target: self
		selector: #inspectActionOfSelectedShortcut)
		icon: (self iconNamed: #smallInspectItIcon).
	^ aMenu
]

{ #category : #private }
SpKeymapPresenter >> setFilter: aFilterText [
	aFilterText
		ifEmpty: [ self collectAllShortcuts ]
		ifNotEmpty: [ :filter | 
			self keymapTable
				items: (self shortcuts select: [ :shortcut | {shortcut realValue shortcut asString . shortcut shortcutName . shortcut categoryName} anySatisfy: [ :s | s includesSubstring: filter caseSensitive: false ] ]) ]
]

{ #category : #private }
SpKeymapPresenter >> shortcuts [
	| collection |
	collection := OrderedCollection new.
	KMRepository default categories
		do: [ :cat | 
			cat keymaps
				do: [ :keymapping | 
					collection
						add:
							(KMShortcutDeclaration new
								shortcutName: keymapping name;
								categoryName: cat name;
								yourself) ] ].
	^ collection sorted: #categoryName ascending
]

{ #category : #accessing }
SpKeymapPresenter >> title [
	^ 'Keymap Browser'
]
