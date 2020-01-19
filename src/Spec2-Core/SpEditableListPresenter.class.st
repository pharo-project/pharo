"
This widget allows you to edit a list of items :
- add / remove an item to/from the list 
- order the list by moving elements up/down/top/bottom.

The default behavior is to work directly on the model collection. It means the collection has to support #add:, #remove: methods (e.g. OrderedCollection).
If you prefer, you can do a copy of the list and give the copy to the EditableList. It will allow the user to accept / reject changes (for example by opening the widget in a DialogWindow) before affecting the original list. If you use a copy of your collection, then it is your responsability to copy EditableList items back to the original list.

The addItemBlock is used to provide a way to give the item to add (e.g. a UIManager default chooseFrom: values:).

Example:
self example
"
Class {
	#name : #SpEditableListPresenter,
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
		'okBlock',
		'label',
		'removeItemBlock'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #spec }
SpEditableListPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
					addLast: #addButton expand: false;
					addLast: #removeButton expand: false;
					yourself)
			expand: false;
		add:
			(SpBoxLayout newHorizontal
				add: #list;
				add:
					(SpBoxLayout newVertical
							add: #topButton expand: false fill: false padding: 0;
							add: #upButton expand: false fill: false padding: 0;
							add: #downButton expand: false fill: false padding: 0;
							add: #bottomButton expand: false fill: false padding: 0;
							yourself)
					expand: false;
				yourself);
		yourself
]

{ #category : #'instance creation' }
SpEditableListPresenter class >> new: aCollection [
	^ self new items: aCollection.
		
]

{ #category : #spec }
SpEditableListPresenter class >> withoutOrderingBar [
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
					addLast: #addButton expand: false;
					addLast: #removeButton expand: false;
					yourself)
			expand: false;
		add: #list;
		yourself
]

{ #category : #accessing }
SpEditableListPresenter >> addButton [
	^ addButton
]

{ #category : #api }
SpEditableListPresenter >> addItemBlock: aBlock [
	addItemBlock := aBlock.
]

{ #category : #accessing }
SpEditableListPresenter >> bottomButton [
	^ bottomButton
]

{ #category : #initialization }
SpEditableListPresenter >> connectPresenters [
	super connectPresenters.
	addButton action: [ 
		| newItem |
		newItem := addItemBlock value.
		newItem ifNotNil: [ 
			self items add: newItem.
			self refresh. ] ].
	removeButton 
		action: [ removeItemBlock cull: self selectedItem ].
	topButton action: [ self moveElementAt: self selectedIndex to: 1 ].
	bottomButton action: [ 
		self moveElementAt: self selectedIndex to: self items size ].
	upButton action: [ 
		self moveElementAt: self selectedIndex to: self selectedIndex - 1 ].
	downButton action: [ 
		self moveElementAt: self selectedIndex to: self selectedIndex + 1 ]
]

{ #category : #api }
SpEditableListPresenter >> display: aBlock [

	list display: aBlock
]

{ #category : #accessing }
SpEditableListPresenter >> downButton [
	^ downButton
]

{ #category : #initialization }
SpEditableListPresenter >> initialize [
	super initialize.
	title := 'Title'.
	okBlock := [  ].
	removeItemBlock := [ :item | 
								self items remove: item.
								self refresh. ]
]

{ #category : #initialization }
SpEditableListPresenter >> initializeDialogWindow: aWindow [

	aWindow 
		addButton: 'Ok' 
		do: [ :presenter | 
			self performOkAction. 
			presenter close ]
]

{ #category : #initialization }
SpEditableListPresenter >> initializePresenters [
	label := self newLabel.
	list := self newList.
	addButton := self newButton.
	removeButton := self newButton.
	upButton := self newButton.
	downButton := self newButton.
	topButton := self newButton.
	bottomButton := self newButton.
	addButton
		addStyle: 'small';
		icon: (self iconNamed: #addIcon);
		help: 'Add a new item to the list'.
	removeButton
		addStyle: 'small';
		icon: (self iconNamed: #removeIcon);
		help: 'Remove a item from the list'.
	upButton
		addStyle: 'small';
		icon: (self iconNamed: #upIcon);
		help: 'Move this item up from one element'.
	downButton
		addStyle: 'small';
		icon: (self iconNamed: #downIcon);
		help: 'Move this item down from one element'.
	topButton
		addStyle: 'small';
		icon: (self iconNamed: #topIcon);
		help: 'Move this item on the first position of the list'.
	bottomButton
		addStyle: 'small';
		icon: (self iconNamed: #bottomIcon);
		help: 'Move this item on the last position of the list'
]

{ #category : #initialization }
SpEditableListPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter title: self title
]

{ #category : #accessing }
SpEditableListPresenter >> items [
	^ list model items
]

{ #category : #private }
SpEditableListPresenter >> items: anItemList [
	list items: anItemList.
]

{ #category : #accessing }
SpEditableListPresenter >> label: aString [
	label label: aString 
]

{ #category : #accessing }
SpEditableListPresenter >> list [
	^ list
]

{ #category : #private }
SpEditableListPresenter >> moveElementAt: index to: newIndex [
	"WARNING: this method can only be used if the model is an OrderedCollection"
	| elementToMove |
	
	(newIndex between: 1 and: self items size) 
		ifFalse: [ ^ self ].
		
	elementToMove := list itemAt: index.
	self items 
		remove: elementToMove;
		add: elementToMove beforeIndex: newIndex.
	
	self refresh.
	self list selectIndex: newIndex
]

{ #category : #api }
SpEditableListPresenter >> okAction: aBlock [

	okBlock := aBlock
]

{ #category : #private }
SpEditableListPresenter >> performOkAction [

	okBlock value
]

{ #category : #api }
SpEditableListPresenter >> refresh [
	self items: self items
]

{ #category : #accessing }
SpEditableListPresenter >> removeButton [
	^ removeButton
]

{ #category : #api }
SpEditableListPresenter >> removeItem: anObject [
	self items remove: anObject.
	list selection unselectAll
]

{ #category : #api }
SpEditableListPresenter >> removeItemBlock: aBlock [
	removeItemBlock := aBlock.
]

{ #category : #api }
SpEditableListPresenter >> selectItem: anIndex [
	list selectItem: anIndex
]

{ #category : #api }
SpEditableListPresenter >> selectedIndex [
	^ list selection selectedIndex
]

{ #category : #api }
SpEditableListPresenter >> selectedItem [
	^ list selection selectedItem
]

{ #category : #accessing }
SpEditableListPresenter >> title [
	^ title
]

{ #category : #accessing }
SpEditableListPresenter >> title: aTitle [
	title := aTitle 
]

{ #category : #accessing }
SpEditableListPresenter >> topButton [
	^ topButton
]

{ #category : #accessing }
SpEditableListPresenter >> upButton [
	^ upButton
]

{ #category : #api }
SpEditableListPresenter >> whenSelectionChangedDo: aBlockClosure [ 
	list whenSelectionChangedDo: aBlockClosure
]
