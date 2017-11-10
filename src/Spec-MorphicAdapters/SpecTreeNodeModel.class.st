"
I am a Tree node especially made for being used by the Spec framework.
"
Class {
	#name : #SpecTreeNodeModel,
	#superclass : #MorphTreeNodeModel,
	#instVars : [
		'nodeModel',
		'icon',
		'childrenItems',
		'hasContents',
		'childrenItemsBlock'
	],
	#category : #Spec-MorphicAdapters-Support
}

{ #category : #compatibility }
SpecTreeNodeModel >> bindKeyCombination: aShortcut toAction: aBlock [
	"Do nothing"
]

{ #category : #building }
SpecTreeNodeModel >> childNodeFromItem: anItem [

	^ (anItem
		container: self model;
		buildWithSpec)
		parentNode: self;
		yourself
]

{ #category : #accessing }
SpecTreeNodeModel >> childrenItems [
	^ self model autoRefreshOnExpand
		ifTrue: [ childrenItemsBlock value ]
		ifFalse: [ childrenItems ifNil: [ childrenItems := childrenItemsBlock value ] ]
]

{ #category : #accessing }
SpecTreeNodeModel >> childrenItems: aCollection [
	childrenItems := aCollection
]

{ #category : #accessing }
SpecTreeNodeModel >> childrenItemsBlock [
	^ childrenItemsBlock
]

{ #category : #accessing }
SpecTreeNodeModel >> childrenItemsBlock: aBlock [
	childrenItemsBlock := aBlock
]

{ #category : #protocol }
SpecTreeNodeModel >> content [
	^ self nodeModel content
]

{ #category : #accessing }
SpecTreeNodeModel >> contents [
	^ self childrenItems collect: [ :child | self childNodeFromItem: child ]
]

{ #category : #protocol }
SpecTreeNodeModel >> hasContentToShow [

	^ self nodeModel hasContentToShow
]

{ #category : #building }
SpecTreeNodeModel >> hasContents [
	hasContents isBlock
		ifTrue: [ ^ hasContents value ].
	^ self childrenItems notEmpty
]

{ #category : #building }
SpecTreeNodeModel >> hasContents: aBlock [

	hasContents := aBlock
]

{ #category : #accessing }
SpecTreeNodeModel >> helpText [

	^ nodeModel help
]

{ #category : #accessing }
SpecTreeNodeModel >> icon [

	^ icon
]

{ #category : #accessing }
SpecTreeNodeModel >> icon: anIcon [

	icon := anIcon
]

{ #category : #protocol }
SpecTreeNodeModel >> isExpanded [

	^ self nodeModel isExpanded
]

{ #category : #protocol }
SpecTreeNodeModel >> isExpanded: aBoolean [

	self nodeModel isExpanded: aBoolean
]

{ #category : #protocol }
SpecTreeNodeModel >> lastClicked: aBoolean [

	self nodeModel lastClicked: aBoolean.
	aBoolean
		ifTrue: [ self model highlightedItem: self nodeModel ]
		ifFalse: [ self model highlightedItem: nil ].
]

{ #category : #events }
SpecTreeNodeModel >> mouseDown: event [

	self nodeModel mouseDownAction cull: event
]

{ #category : #accessing }
SpecTreeNodeModel >> nodeModel [
	^ nodeModel
]

{ #category : #accessing }
SpecTreeNodeModel >> nodeModel: anObject [
	nodeModel := anObject
]

{ #category : #protocol }
SpecTreeNodeModel >> selected [

	^ self nodeModel selected
]

{ #category : #protocol }
SpecTreeNodeModel >> selected: aBoolean [

	self nodeModel selected: aBoolean
]

{ #category : #accessing }
SpecTreeNodeModel >> setChildren: aBlock [
	"Store a aBlock which will return the children when evaluated."

	self childrenItemsBlock: aBlock
]
