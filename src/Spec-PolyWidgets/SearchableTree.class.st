"
I am a tree plus an input field to be able to filter the tree elements
"
Class {
	#name : #SearchableTree,
	#superclass : #ComposablePresenter,
	#instVars : [
		'inputField',
		'roots',
		'tree',
		'label',
		'button'
	],
	#category : #'Spec-PolyWidgets-ListAndTree'
}

{ #category : #specs }
SearchableTree class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newRow: [:r |
			r
			"add: #label width: 50;"
			add: #inputField;
			add: #button width: 25 ] height: 25;
		newRow: #tree top: 25;
		yourself
]

{ #category : #examples }
SearchableTree class >> example [
	<sampleInstance>
	^ self new
		openWithSpec
]

{ #category : #specs }
SearchableTree class >> searchAtBottom [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [:c |
			c 
				add: #tree;
				newRow: [:r |
					r
					"add: #label width: 50;"
					add: #inputField;
					add: #button width: 25 ] height: 25 ];
		yourself
]

{ #category : #api }
SearchableTree >> autoAccept: aBoolean [

	inputField autoAccept: aBoolean
]

{ #category : #api }
SearchableTree >> autoMultiSelection: aBoolean [

	tree autoMultiSelection: aBoolean
]

{ #category : #accessing }
SearchableTree >> button [

	^ button
]

{ #category : #'api-compatibility' }
SearchableTree >> childrenBlock [
	
	^ tree childrenBlock
]

{ #category : #'api-compatibility' }
SearchableTree >> childrenBlock: aBlock [
	
	tree childrenBlock: aBlock
]

{ #category : #api }
SearchableTree >> columnInset: anInteger [

	tree columnInset: anInteger
]

{ #category : #api }
SearchableTree >> columns [
	
	^ tree columns
]

{ #category : #api }
SearchableTree >> columns: aCollection [
	
	tree columns: aCollection
]

{ #category : #'api-compatibility' }
SearchableTree >> displayBlock [

	^ tree displayBlock
]

{ #category : #'api-compatibility' }
SearchableTree >> displayBlock: aBlock [
	
	tree displayBlock: aBlock
]

{ #category : #api }
SearchableTree >> doubleClick: aBlock [

	tree doubleClick: aBlock
]

{ #category : #api }
SearchableTree >> evenRowColor: aColor [

	tree evenRowColor: aColor
]

{ #category : #api }
SearchableTree >> highlightedItem: aTreeNodePresenter [
	
	tree highlightedItem: aTreeNodePresenter
]

{ #category : #api }
SearchableTree >> iconBlock: aBlock [
	
	tree iconBlock: aBlock
]

{ #category : #initialization }
SearchableTree >> initialize [

	super initialize.

	roots := OrderedCollection new asValueHolder
]

{ #category : #initialization }
SearchableTree >> initializePresenter [

	inputField whenTextChanged: [ :text | self updateRootFor: text ].
		
	tree whenTreeUpdated: [ inputField text: '' ].
]

{ #category : #initialization }
SearchableTree >> initializeWidgets [

	tree := self newTree.
	inputField := self newTextInput.
	label := self newLabel.
	button := self newButton.
			
	tree rootNodeHolder: [ :item | 
		(tree defaultNodeFor: item)
			matchTokenFilterBlock: [ :aFilter |
				aFilter token
					ifEmpty: [ true ]
					ifNotEmpty: [ item asString asLowercase includesSubstring: aFilter token asLowercase ] ];
			yourself ].
			
	button 
		label: '';
		icon: (self iconNamed: #smallFindIcon);
		state: false;
		action: [ inputField accept ].
			
	label 
		label: 'Search:';
		color:Color black.
	
	inputField
		ghostText: 'Search';
		entryCompletion: nil;
		acceptOnCR: true.
		
	self focusOrder
		add: tree;
		add: inputField
]

{ #category : #accessing }
SearchableTree >> inputField [
	^ inputField
]

{ #category : #api }
SearchableTree >> isCheckList: aBoolean [

	tree isCheckList: aBoolean
]

{ #category : #api }
SearchableTree >> keyStrokeAction: aBlock [

	tree keyStrokeAction: aBlock
]

{ #category : #accessing }
SearchableTree >> label [
	^ label
]

{ #category : #api }
SearchableTree >> menu [
	
	^ tree menu
]

{ #category : #api }
SearchableTree >> menu: aBlock [
	
	tree menu: aBlock
]

{ #category : #api }
SearchableTree >> multiSelection: aBoolean [

	tree multiSelection: aBoolean
]

{ #category : #api }
SearchableTree >> preferedPaneColor: aColor [

	tree preferedPaneColor: aColor
]

{ #category : #api }
SearchableTree >> resetSelection [

	tree resetSelection
]

{ #category : #api }
SearchableTree >> resizerWidth: anInteger [

	tree resizerWidth: anInteger
]

{ #category : #api }
SearchableTree >> rootNodeHolder [

	^ tree rootNodeHolder
]

{ #category : #api }
SearchableTree >> rootNodeHolder: aBlock [

	tree rootNodeHolder: aBlock
]

{ #category : #api }
SearchableTree >> roots [
	
	^ tree roots
]

{ #category : #api }
SearchableTree >> roots: aCollection [

	roots value: aCollection asOrderedCollection.	
	tree roots: aCollection asOrderedCollection
]

{ #category : #api }
SearchableTree >> rowInset: anInteger [

	tree rowInset: anInteger
]

{ #category : #api }
SearchableTree >> secondSelection: anItem [
	
	tree secondSelection: anItem
]

{ #category : #api }
SearchableTree >> selectedItem [
	
	^ tree selectedItem
]

{ #category : #api }
SearchableTree >> selectedItem: anItem [
	
	tree selectedItem: anItem
]

{ #category : #api }
SearchableTree >> selectedItems [
	
	^ tree selectedItems
]

{ #category : #api }
SearchableTree >> selectedItems: aCollection [
	
	tree selectedItems: aCollection
]

{ #category : #accessing }
SearchableTree >> tree [
	^ tree
]

{ #category : #updating }
SearchableTree >> updateRootFor: aText [
	| aFilter |
	
	aFilter := TokenTreeFilter token: aText.
	tree filterWith: aFilter
]

{ #category : #api }
SearchableTree >> updateTree [
	
	tree updateTree
]

{ #category : #'api-events-compatibility' }
SearchableTree >> whenChildrenBlockChanged: aBlock [
	
	tree whenChildrenBlockChanged: aBlock
]

{ #category : #'api-events-compatibility' }
SearchableTree >> whenDisplayBlockChanged: aBlock [
	
	tree whenDisplayBlockChanged: aBlock
]

{ #category : #'api-events' }
SearchableTree >> whenMenuChanged: aBlock [
	
	tree whenMenuChanged: aBlock
]

{ #category : #'api-events' }
SearchableTree >> whenRootsChanged: aBlock [

	roots whenChangedDo: aBlock
]

{ #category : #'list api-events' }
SearchableTree >> whenSelectedItemChanged: aBlock [

	tree whenSelectedItemChanged: aBlock
]

{ #category : #'api-events' }
SearchableTree >> whenSelectedItemsChanged: aBlock [
	
	tree whenSelectedItemsChanged: aBlock
]
