"
A KMDescription is a window showing the description of shortcuts for a specified set of symbols reprensenting KMCategories
"
Class {
	#name : #KMDescriptionPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'shortcutList',
		'collectingBlock',
		'categories',
		'orderingList',
		'sortingBlock',
		'updatingBlock',
		'okToolbar',
		'categoryList'
	],
	#category : #'Keymapping-Tools-Spec'
}

{ #category : #specs }
KMDescriptionPresenter class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		newColumn: [:c | 
			c 
				newRow: [:r | 
					r 
						add: #categoryList;
						add: #orderingList ] height: 25;
				add: #shortcutList;
				add: #okToolbar height: 25 ]
]

{ #category : #example }
KMDescriptionPresenter class >> example [

	| d |
	d := self new.
	d categories: #(GlobalShortcuts MonticelloShortcuts TextEditor).
	d openWithSpec
]

{ #category : #specs }
KMDescriptionPresenter class >> title [

	^ 'Shortcuts description'
]

{ #category : #accessing }
KMDescriptionPresenter >> categories [
	
	^ categories
]

{ #category : #api }
KMDescriptionPresenter >> categories: aCollectionOfSymbols [
	| cats |
	
	cats := aCollectionOfSymbols collect: [:e | KMRepository default categoryForName: e].
	categories value: cats.
	self updateShortcutList
]

{ #category : #accessing }
KMDescriptionPresenter >> categoryList [
	
	^ categoryList
]

{ #category : #initialization }
KMDescriptionPresenter >> collectDescriptionShortcut [
	collectingBlock := [ :e | {(e description asString) . ':' . (e shortcut asString)} ]
]

{ #category : #initialization }
KMDescriptionPresenter >> collectShortcutDescription [
	collectingBlock := [:e | {e shortcut asString. ':'. e description asString} ]
]

{ #category : #initialization }
KMDescriptionPresenter >> initCategoryList: aCollection [
	categoryList emptyList.
	aCollection do: [ :cat | categoryList addItemLabeled: cat name do: [ self updateShortcutList ] ]
]

{ #category : #private }
KMDescriptionPresenter >> initialExtent [

	^  (600@350)
]

{ #category : #initialization }
KMDescriptionPresenter >> initialize [

	categories := nil asValueHolder.
	sortingBlock := [:a :b | a first < b first ].
	super initialize.
      self collectShortcutDescription

]

{ #category : #initialization }
KMDescriptionPresenter >> initializePresenter [
		
	categories whenChangedDo: [:col | self initCategoryList:col ].
	okToolbar okAction: [ self delete ].
]

{ #category : #initialization }
KMDescriptionPresenter >> initializeWidgets [

	categoryList := self newDropList.
	shortcutList := self newMultiColumnList.
	okToolbar := self instantiate: OkToolbar.
	orderingList := self newDropList.
	
	orderingList 
		addItemLabeled:'shortcut	:	description' 
			do: [self collectShortcutDescription.
				 self updateShortcutList];
		addItemLabeled:'description	:	shortcut' 
			do: [self collectDescriptionShortcut.
				 self updateShortcutList].
	
	shortcutList 
		displayBlock: [ :e | e ];
		allowToSelect: false.

	okToolbar okButton
			label: 'Close';
			state: false
]

{ #category : #accessing }
KMDescriptionPresenter >> okToolbar [
	
	^ okToolbar
]

{ #category : #accessing }
KMDescriptionPresenter >> orderingList [
	
	^ orderingList
]

{ #category : #accessing }
KMDescriptionPresenter >> shortcutList [
	
	^ shortcutList
]

{ #category : #initialization }
KMDescriptionPresenter >> updateShortcutList [
	| cat items |
	cat := categories value at: self categoryList selectedIndex.
	items := (cat entriesAt: #all) keymaps collect: [ :e | collectingBlock value: e ].
	shortcutList items: (items sorted: sortingBlock)
]
