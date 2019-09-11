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
	#name : #SpEditableList,
	#superclass : #SpPresenter,
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
	#category : #'Spec2-PolyWidgets-ListAndTree'
}

{ #category : #spec }
SpEditableList class >> defaultSpec [
	^ SpLayout composed
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

{ #category : #'instance creation' }
SpEditableList class >> new: aCollection [
	^ self new list: aCollection copy.
		
]

{ #category : #accessing }
SpEditableList >> addButton [
	^ addButton
]

{ #category : #api }
SpEditableList >> addItemBlock: aBlock [
	addItemBlock := aBlock.
]

{ #category : #accessing }
SpEditableList >> bottomButton [
	^ bottomButton
]

{ #category : #accessing }
SpEditableList >> downButton [
	^ downButton
]

{ #category : #initialization }
SpEditableList >> initialize [
	super initialize.
	title := 'Title'.
	okBlock := [  ]
]

{ #category : #initialization }
SpEditableList >> initializeDialogWindow: aWindow [

	aWindow 
		addButton: 'Ok' 
		do: [ :presenter | 
			self performOkAction. 
			presenter close ]
]

{ #category : #initialization }
SpEditableList >> initializePresenter [
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
SpEditableList >> initializeWidgets [
	list := self newList.
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
SpEditableList >> list [
	^ list
]

{ #category : #private }
SpEditableList >> list: aList [
	list items: aList.
]

{ #category : #private }
SpEditableList >> moveElementAt: index to: newIndex [
	| elementToMove orderedList |
	(newIndex < 1 or: [ newIndex > list items size ])
		ifTrue: [ ^ self ].
	elementToMove := list itemAt: index.
	orderedList := list items copy asOrderedCollection
		removeAt: index;
		add: elementToMove beforeIndex: newIndex;
		yourself.
	self list: orderedList.
	self list selectIndex: newIndex
]

{ #category : #api }
SpEditableList >> okAction: aBlock [

	okBlock := aBlock
]

{ #category : #private }
SpEditableList >> performOkAction [

	okBlock value
]

{ #category : #accessing }
SpEditableList >> removeButton [
	^ removeButton
]

{ #category : #accessing }
SpEditableList >> title [
	^ title
]

{ #category : #accessing }
SpEditableList >> title: aTitle [
	title := aTitle 
]

{ #category : #accessing }
SpEditableList >> topButton [
	^ topButton
]

{ #category : #accessing }
SpEditableList >> upButton [
	^ upButton
]
