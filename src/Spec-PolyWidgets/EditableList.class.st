"
This widget allows you to edit a list of items :
- add / remove an item to/from the list 
- order the list by moving elements up/down/top/bottom.

The default behavior is to do a copy of the list. The widget works with its internal copy. It allows the user to accept / reject changes (for example by opening the widget in a DialogWindow) before affecting the original list. It is your responsability to copy EditableList items back to the original list.

The addItemBlock is used to provide a way to give the item to add (e.g. a UIManager default chooseFrom: values:).

Example:
self example
"
Class {
	#name : #EditableList,
	#superclass : #ComposablePresenter,
	#instVars : [
		'title',
		'list',
		'addButton',
		'removeButton',
		'upButton',
		'downButton',
		'topButton',
		'bottomButton',
		'addItemBlock',
		'okBlock'
	],
	#category : #Spec-PolyWidgets-ListAndTree
}

{ #category : #example }
EditableList class >> example [
	^self new
	
		title: 'Selectors list';
		list: self selectors;
	 openWithSpec
]

{ #category : #'instance creation' }
EditableList class >> new: aCollection [
	^ self new list: aCollection copy.
		
]

{ #category : #spec }
EditableList class >> spec [
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :column | 
			column
				newRow: [ :menuRow | 
							menuRow
								add: #addButton;
								add: #removeButton ]
					height: 25;
				newRow: [ :listRow | 
							listRow
								newColumn: [ :c1 | c1 add: #list ];
								newColumn: [ :c2 | 
											c2
												add: #topButton;
												add: #upButton;
												add: #downButton;
												add: #bottomButton ]
									width: 24 ] ] 
		yourself
]

{ #category : #accessing }
EditableList >> addButton [
	^ addButton
]

{ #category : #api }
EditableList >> addItemBlock: aBlock [
	addItemBlock := aBlock.
]

{ #category : #accessing }
EditableList >> bottomButton [
	^ bottomButton
]

{ #category : #accessing }
EditableList >> downButton [
	^ downButton
]

{ #category : #initialization }
EditableList >> initialize [
	"Initialization code for EditableList"

	title := 'Title' asValueHolder.
	
	super initialize.
	okBlock := [ ].
]

{ #category : #initialization }
EditableList >> initializeDialogWindow: aWindow [
	super initializeDialogWindow: aWindow.
	aWindow okAction: [ self performOkAction ]
]

{ #category : #initialization }
EditableList >> initializePresenter [
	super initializePresenter
	
	addButton action: [ | requirement | 
						requirement := addItemBlock value.
						requirement ifNotNil: [ 
							self list: (self list listItems copyWith: requirement) ] ].

	removeButton
		action: [ self list: (list listItems copyWithoutIndex: list selectedIndex) ].
		
	topButton
		action: [ self moveElementAt: list selectedIndex to: 1 ].
	bottomButton
		action: [ self moveElementAt: list selectedIndex to: list listItems size ].
	upButton
		action: [ self moveElementAt: list selectedIndex to: list selectedIndex - 1].
	downButton
		action: [ self moveElementAt: list selectedIndex to: list selectedIndex + 1].

]

{ #category : #initialization }
EditableList >> initializeWidgets [
	list := self instantiate: FastTablePresenter.
	addButton := self newButton.
	removeButton := self newButton.
	upButton := self newButton.
	downButton := self newButton.
	topButton := self newButton.
	bottomButton := self newButton.
	addButton
		icon: (self iconNamed: #addIcon);
		help: 'Add a new item to the list'.
	removeButton
		icon: (self iconNamed: #deleteIcon);
		help: 'Remove a item from the list'.
	upButton
		icon: (self iconNamed: #upIcon);
		help: 'Move this item up from one element'.
	downButton
		icon: (self iconNamed: #downIcon);
		help: 'Move this item down from one element'.
	topButton
		icon: (self iconNamed: #topIcon);
		help: 'Move this item on the first position of the list'.
	bottomButton
		icon: (self iconNamed: #bottomIcon);
		help: 'Move this item on the last position of the list'
]

{ #category : #accessing }
EditableList >> list [
	^ list
]

{ #category : #private }
EditableList >> list: aList [
	list items: aList.
]

{ #category : #private }
EditableList >> moveElementAt: index to: newIndex [
	| elementToMove orderedList |
	
	(newIndex < 1 or: [ newIndex > list listItems size ]) ifTrue: [ ^self ].
	
	elementToMove := list listItems at: index.	
	orderedList := list listItems asOrderedCollection
		removeAt: index;
		add: elementToMove beforeIndex: newIndex;
		yourself.
	self list: orderedList.
	self list setSelectedIndex: newIndex.
]

{ #category : #api }
EditableList >> okAction: aBlock [

	okBlock := aBlock
]

{ #category : #private }
EditableList >> performOkAction [

	okBlock value
]

{ #category : #accessing }
EditableList >> removeButton [
	^ removeButton
]

{ #category : #accessing }
EditableList >> title [
	^ title value
]

{ #category : #accessing }
EditableList >> title: aTitle [
	title := aTitle 
]

{ #category : #accessing }
EditableList >> topButton [
	^ topButton
]

{ #category : #accessing }
EditableList >> upButton [
	^ upButton
]
