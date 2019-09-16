"
I am a tree plus an input field to be able to filter the tree elements
"
Class {
	#name : #SpSearchableTree,
	#superclass : #SpPresenter,
	#instVars : [
		'#inputField',
		'#roots => SpObservableSlot',
		'#tree',
		'#button'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpSearchableTree class >> defaultSpec [
	^ self searchAtBottom
]

{ #category : #specs }
SpSearchableTree class >> searchAtBottom [
	<spec>
	^ SpBoxLayout newVertical
		add: #tree;
		add:
			(SpBoxLayout newHorizontal
				add: #inputField;
				add: #button withConstraints: [ :constraints | constraints width: 25 ])
			withConstraints: [ :constraints | constraints height: 25 ];
		yourself
]

{ #category : #specs }
SpSearchableTree class >> searchAtTop [
	<spec>
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
				add: #inputField;
				add: #button withConstraints: [ :constraints | constraints width: 25 ])
			withConstraints: [ :constraints | constraints height: 25 ];
		add: #tree;
		yourself
]

{ #category : #api }
SpSearchableTree >> autoAccept: aBoolean [

	inputField autoAccept: aBoolean
]

{ #category : #api }
SpSearchableTree >> autoMultiSelection: aBoolean [

	tree autoMultiSelection: aBoolean
]

{ #category : #accessing }
SpSearchableTree >> button [

	^ button
]

{ #category : #'api-compatibility' }
SpSearchableTree >> childrenBlock [
	
	^ tree childrenBlock
]

{ #category : #'api-compatibility' }
SpSearchableTree >> childrenBlock: aBlock [
	
	tree childrenBlock: aBlock
]

{ #category : #api }
SpSearchableTree >> columnInset: anInteger [

	tree columnInset: anInteger
]

{ #category : #api }
SpSearchableTree >> columns [
	
	^ tree columns
]

{ #category : #api }
SpSearchableTree >> columns: aCollection [
	
	tree columns: aCollection
]

{ #category : #initialization }
SpSearchableTree >> connectPresenters [
	inputField whenTextChangedDo: [ :text | self updateRootFor: text ].
	tree whenTreeUpdatedDo: [ inputField text: '' ]
]

{ #category : #'api-compatibility' }
SpSearchableTree >> display [

	^ tree display
]

{ #category : #'api-compatibility' }
SpSearchableTree >> display: aBlock [
	
	tree display: aBlock
]

{ #category : #'api-compatibility' }
SpSearchableTree >> displayBlock [

	self
		deprecated: 'Please use #display instead'
		transformWith: '`@receiver displayBlock' 
						-> '`@receiver display'.

	^ tree display
]

{ #category : #'api-compatibility' }
SpSearchableTree >> displayBlock: aBlock [
	
	self
		deprecated: 'Please use #display: instead'
		transformWith: '`@receiver displayBlock: `@arg' 
						-> '`@receiver display: `@arg'.
	
	tree display: aBlock
]

{ #category : #api }
SpSearchableTree >> doubleClick: aBlock [

	tree doubleClick: aBlock
]

{ #category : #api }
SpSearchableTree >> evenRowColor: aColor [

	tree evenRowColor: aColor
]

{ #category : #api }
SpSearchableTree >> highlightedItem: aTreeNodePresenter [
	
	tree highlightedItem: aTreeNodePresenter
]

{ #category : #api }
SpSearchableTree >> iconBlock: aBlock [
	
	tree iconBlock: aBlock
]

{ #category : #initialization }
SpSearchableTree >> initialize [
	super initialize.

	roots := OrderedCollection new
]

{ #category : #initialization }
SpSearchableTree >> initializePresenters [
	tree := self newTree.
	inputField := self newTextInput.
	button := self newButton.
	tree
		rootNodeHolder: [ :item | 
			(tree defaultNodeFor: item)
				matchTokenFilterBlock: [ :aFilter | 
					aFilter token
						ifEmpty: [ true ]
						ifNotEmpty: [ item asString asLowercase
								includesSubstring: aFilter token asLowercase ] ];
				yourself ].
	button
		label: '';
		icon: (self iconNamed: #smallFindIcon);
		state: false;
		action: [ inputField accept ].
	inputField
		placeholder: 'Search';
		entryCompletion: nil;
		acceptOnCR: true.
	self focusOrder
		add: tree;
		add: inputField
]

{ #category : #accessing }
SpSearchableTree >> inputField [
	^ inputField
]

{ #category : #api }
SpSearchableTree >> isCheckList: aBoolean [

	tree isCheckList: aBoolean
]

{ #category : #api }
SpSearchableTree >> keyStrokeAction: aBlock [

	tree keyStrokeAction: aBlock
]

{ #category : #api }
SpSearchableTree >> menu [
	
	^ tree menu
]

{ #category : #api }
SpSearchableTree >> menu: aBlock [
	
	tree menu: aBlock
]

{ #category : #api }
SpSearchableTree >> multiSelection: aBoolean [

	tree multiSelection: aBoolean
]

{ #category : #api }
SpSearchableTree >> preferedPaneColor: aColor [

	tree preferedPaneColor: aColor
]

{ #category : #api }
SpSearchableTree >> resetSelection [

	tree resetSelection
]

{ #category : #api }
SpSearchableTree >> resizerWidth: anInteger [

	tree resizerWidth: anInteger
]

{ #category : #api }
SpSearchableTree >> rootNodeHolder [

	^ tree rootNodeHolder
]

{ #category : #api }
SpSearchableTree >> rootNodeHolder: aBlock [

	tree rootNodeHolder: aBlock
]

{ #category : #api }
SpSearchableTree >> roots [
	
	^ tree roots
]

{ #category : #api }
SpSearchableTree >> roots: aCollection [
	roots := aCollection asOrderedCollection.
	tree roots: aCollection asOrderedCollection
]

{ #category : #api }
SpSearchableTree >> rowInset: anInteger [

	tree rowInset: anInteger
]

{ #category : #api }
SpSearchableTree >> secondSelection: anItem [
	
	tree secondSelection: anItem
]

{ #category : #api }
SpSearchableTree >> selectedItem [
	
	^ tree selectedItem
]

{ #category : #api }
SpSearchableTree >> selectedItem: anItem [
	
	tree selectedItem: anItem
]

{ #category : #api }
SpSearchableTree >> selectedItems [
	
	^ tree selectedItems
]

{ #category : #api }
SpSearchableTree >> selectedItems: aCollection [
	
	tree selectedItems: aCollection
]

{ #category : #accessing }
SpSearchableTree >> tree [
	^ tree
]

{ #category : #updating }
SpSearchableTree >> updateRootFor: aText [
	| aFilter |
	
	aFilter := SpTokenTreeFilter token: aText.
	tree filterWith: aFilter
]

{ #category : #api }
SpSearchableTree >> updateTree [
	
	tree updateTree
]

{ #category : #'api-events-compatibility' }
SpSearchableTree >> whenChildrenBlockChanged: aBlock [
	
	tree whenChildrenBlockChanged: aBlock
]

{ #category : #'api-events-compatibility' }
SpSearchableTree >> whenDisplayBlockChanged: aBlock [
	
	tree whenDisplayBlockChanged: aBlock
]

{ #category : #'api-events' }
SpSearchableTree >> whenMenuChanged: aBlock [
	
	tree whenMenuChanged: aBlock
]

{ #category : #'api-events' }
SpSearchableTree >> whenRootsChanged: aBlock [
	self property: #roots whenChangedDo: aBlock
]

{ #category : #'list api-events' }
SpSearchableTree >> whenSelectedItemChanged: aBlock [

	tree whenSelectedItemChanged: aBlock
]

{ #category : #'api-events' }
SpSearchableTree >> whenSelectedItemsChanged: aBlock [
	
	tree whenSelectedItemsChanged: aBlock
]
