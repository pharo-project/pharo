"
A KeymapBrowser  shows a list of all defined keymappings.
It shows a filterable list of keymap shortcuts,  name and categories.
Use the context menu to browse the method that defines the shortcut.
"
Class {
	#name : #KeymapBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'clearFilterButton',
		'filterField',
		'kmTable'
	],
	#category : #'Spec-Tools-Keymapping'
}

{ #category : #specs }
KeymapBrowser class >> defaultSpec [
	<spec>
	^ SpecLayout composed
		newColumn: [ :c | c newRow: [ :r | 
							r
								add: #filterField;
								add: #clearFilterButton width: 25 ]
						height: self toolbarHeight.
					c newRow: [ :r | r add: #kmTable ] ];
		yourself
]

{ #category : #'system menu' }
KeymapBrowser class >> keymapBrowserMenuOn: aBuilder [ 
	"I build a menu"
	<worldMenu>
	(aBuilder item: #'Keymap Browser')
		parent: #System;
		order: 4;
		icon: self taskbarIcon;
		action: [ self new openWithSpec ];
		help: 'List all know shortcuts and Keymapping categories.'.
]

{ #category : #accessing }
KeymapBrowser class >> taskbarIconName [
	^ #keymapBrowserIcon
]

{ #category : #private }
KeymapBrowser >> browseDefinitionOfSelectedShortcut [
	| selection action class selector |
	selection := self kmTable selectedItem.
	selection ifNil: [ ^ self ].
	action := selection realValue keymap action.
	action ifNil: [ ^ self ].
	class := action method methodClass.
	selector := action method selector.
	Smalltalk tools browser openOnClass: class selector: selector highlight: selection shortcutName
]

{ #category : #accessing }
KeymapBrowser >> clearFilterButton [
	^ clearFilterButton
]

{ #category : #accessing }
KeymapBrowser >> clearFilterButton: anObject [
	clearFilterButton := anObject
]

{ #category : #private }
KeymapBrowser >> collectAllShortcuts [
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
	self kmTable items: collection.
]

{ #category : #accessing }
KeymapBrowser >> filterField [
	^ filterField
]

{ #category : #accessing }
KeymapBrowser >> filterField: anObject [
	filterField := anObject
]

{ #category : #protocol }
KeymapBrowser >> initialExtent [
	^ 600@300
]

{ #category : #initialization }
KeymapBrowser >> initializePresenter [
	filterField
		whenTextChanged: [ :text | 
			clearFilterButton enabled: text isNotEmpty.
			self setFilter: text ].
	clearFilterButton action: [ filterField text: '' ]
]

{ #category : #initialization }
KeymapBrowser >> initializeWidgets [
	self title: 'Keymap Browser'.
	kmTable := self newMultiColumnList.
	kmTable displayBlock: [ :item | self wrapItem: item ].
	kmTable sortingBlock: [ :a :b | a categoryName < b categoryName ].
	kmTable menu: [ :m :s| self menu:m shifted:s].
	filterField := self newTextInput.
	filterField removeEntryCompletion.
	filterField ghostText: 'Filter'.
	filterField autoAccept: true.
	clearFilterButton := self newButton.
	clearFilterButton icon: (self iconNamed: #smallCancelIcon).
	clearFilterButton enabled:false.
	self collectAllShortcuts
]

{ #category : #private }
KeymapBrowser >> inspectActionOfSelectedShortcut [
	| selection action   |
	selection := self kmTable selectedItem.
	selection ifNil: [ ^ self ].
	action := selection realValue keymap.
	action ifNil: [ ^ self ].
	action inspect
]

{ #category : #accessing }
KeymapBrowser >> kmTable [
	^ kmTable
]

{ #category : #accessing }
KeymapBrowser >> kmTable: anObject [
	kmTable := anObject
]

{ #category : #private }
KeymapBrowser >> menu: aMenu shifted: aBoolean [
	(aMenu add: 'Refresh' target: self selector: #collectAllShortcuts) 
	icon: (self iconNamed: #smallUpdateIcon).
	self kmTable selectedItem ifNil:[^ aMenu].
	(aMenu add: 'Browse Definition' target: self selector: #browseDefinitionOfSelectedShortcut)
	icon: (self iconNamed: #smallSystemBrowserIcon).
	(aMenu add: 'Inspect Action' target: self selector: #inspectActionOfSelectedShortcut)
	icon: (self iconNamed: #smallInspectItIcon).
	^ aMenu
]

{ #category : #private }
KeymapBrowser >> setFilter: aFilterText [
	aFilterText isEmpty
		ifTrue: [ self kmTable resetFilteringBlock ]
		ifFalse: [ self kmTable
				filteringBlock: [ :items | items select: [ :item | 
						(self wrapItem: item) anySatisfy: [ :s | s includesSubstring: aFilterText caseSensitive: false ] ] ] ]
]

{ #category : #private }
KeymapBrowser >> wrapItem: aKMShortcutDeclaration [
	| name categorie |
	name := aKMShortcutDeclaration shortcutName.
	categorie := aKMShortcutDeclaration categoryName.
	^ {aKMShortcutDeclaration realValue shortcut asString . name.
	categorie}
]
