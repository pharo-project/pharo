"
I am a Tree node especially made for being used by the Spec framework.
"
Class {
	#name : #SpTreeNodeModel,
	#superclass : #MorphTreeNodeModel,
	#instVars : [
		'nodeModel',
		'icon',
		'childrenItems',
		'hasContents',
		'childrenItemsBlock'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #compatibility }
SpTreeNodeModel >> bindKeyCombination: aShortcut toAction: aBlock [
	"Do nothing"
]

{ #category : #building }
SpTreeNodeModel >> childNodeFromItem: anItem [

	^ (anItem
		container: self model;
		buildWithSpec)
		parentNode: self;
		yourself
]

{ #category : #accessing }
SpTreeNodeModel >> childrenItems [
	^ self model autoRefreshOnExpand
		ifTrue: [ childrenItemsBlock value ]
		ifFalse: [ childrenItems ifNil: [ childrenItems := childrenItemsBlock value ] ]
]

{ #category : #accessing }
SpTreeNodeModel >> childrenItems: aCollection [
	childrenItems := aCollection
]

{ #category : #accessing }
SpTreeNodeModel >> childrenItemsBlock [
	^ childrenItemsBlock
]

{ #category : #accessing }
SpTreeNodeModel >> childrenItemsBlock: aBlock [
	childrenItemsBlock := aBlock
]

{ #category : #protocol }
SpTreeNodeModel >> content [
	^ self nodeModel content
]

{ #category : #accessing }
SpTreeNodeModel >> contents [
	^ self childrenItems collect: [ :child | self childNodeFromItem: child ]
]

{ #category : #protocol }
SpTreeNodeModel >> hasContentToShow [

	^ self nodeModel hasContentToShow
]

{ #category : #building }
SpTreeNodeModel >> hasContents [
	hasContents isBlock
		ifTrue: [ ^ hasContents value ].
	^ self childrenItems notEmpty
]

{ #category : #building }
SpTreeNodeModel >> hasContents: aBlock [

	hasContents := aBlock
]

{ #category : #accessing }
SpTreeNodeModel >> helpText [

	^ nodeModel help
]

{ #category : #accessing }
SpTreeNodeModel >> icon [

	^ icon
]

{ #category : #accessing }
SpTreeNodeModel >> icon: anIcon [

	icon := anIcon
]

{ #category : #protocol }
SpTreeNodeModel >> isExpanded [

	^ self nodeModel isExpanded
]

{ #category : #protocol }
SpTreeNodeModel >> isExpanded: aBoolean [

	self nodeModel isExpanded: aBoolean
]

{ #category : #protocol }
SpTreeNodeModel >> lastClicked: aBoolean [

	self nodeModel lastClicked: aBoolean.
	aBoolean
		ifTrue: [ self model highlightedItem: self nodeModel ]
		ifFalse: [ self model highlightedItem: nil ].
]

{ #category : #events }
SpTreeNodeModel >> mouseDown: event [

	self nodeModel mouseDownAction cull: event
]

{ #category : #accessing }
SpTreeNodeModel >> nodeModel [
	^ nodeModel
]

{ #category : #accessing }
SpTreeNodeModel >> nodeModel: anObject [
	nodeModel := anObject
]

{ #category : #protocol }
SpTreeNodeModel >> selected [

	^ self nodeModel selected
]

{ #category : #protocol }
SpTreeNodeModel >> selected: aBoolean [

	self nodeModel selected: aBoolean
]

{ #category : #accessing }
SpTreeNodeModel >> setChildren: aBlock [
	"Store a aBlock which will return the children when evaluated."

	self childrenItemsBlock: aBlock
]

{ #category : #accessing }
SpTreeNodeModel >> visible: aBoolean [

	"Do nothing. Morphs implement visible but this column, though it inherits from Morphic column and has an adapter, dos not."
]
