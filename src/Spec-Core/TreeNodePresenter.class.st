"
I represent a node in a tree widget.
I hold an item, an icon and the list of my children.

In addition, I have a filtering mechanism
"
Class {
	#name : #TreeNodePresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'children',
		'content',
		'icon',
		'container',
		'matchTokenFilter',
		'contents',
		'parentNode',
		'childNodeClass',
		'selected',
		'lastClicked',
		'mouseDownAction',
		'hasContentToShow',
		'isExpanded',
		'hasChildren'
	],
	#category : #Spec-Core-Widgets
}

{ #category : #specs }
TreeNodePresenter class >> adapterName [

	^ #TreeNodeAdapter
]

{ #category : #specs }
TreeNodePresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(TreeNodeAdapter
		adapt: #(model))
]

{ #category : #api }
TreeNodePresenter >> allSelectedItems [
	| result |
	
	result := OrderedCollection new.
	self selected ifTrue: [ result add: self ].
	self children value do: [ :e | 
		result addAll: e allSelectedItems ].
	^ result
]

{ #category : #api }
TreeNodePresenter >> allUnselectedItems [
	| result |
	
	result := OrderedCollection new.
	self selected ifFalse: [ result add: self ].
	self children value do: [ :e | 
		result addAll: e allUnselectedItems ].
	^ result
]

{ #category : #api }
TreeNodePresenter >> buildChildren [

	contents := self children value collect: [ :each || node | 
		node := each isTreeNodeModel
			ifTrue: [ each ]
			ifFalse: [  (self childNodeClassFor: each) new
				content: each  ].
		node 
			parentNode: self;
			yourself ].
		
	^ contents
]

{ #category : #api }
TreeNodePresenter >> childNodeClass [

	^ childNodeClass value
]

{ #category : #api }
TreeNodePresenter >> childNodeClass: aBlock [

	childNodeClass value: aBlock
]

{ #category : #api }
TreeNodePresenter >> childNodeClassFor: anItem [

	^ childNodeClass value
]

{ #category : #api }
TreeNodePresenter >> children [

	^ children value
]

{ #category : #api }
TreeNodePresenter >> children: aBlock [

	children value: aBlock
]

{ #category : #api }
TreeNodePresenter >> container [

	^ container value
]

{ #category : #api }
TreeNodePresenter >> container: anObject [

	container value: anObject
]

{ #category : #api }
TreeNodePresenter >> content [

	^ content value
]

{ #category : #api }
TreeNodePresenter >> content: anObject [

	content value: anObject
]

{ #category : #filtering }
TreeNodePresenter >> contents [
	^ contents ifNil: [ 
		self buildChildren.
		contents ]
]

{ #category : #api }
TreeNodePresenter >> hasChildren [

	^ hasChildren value
]

{ #category : #api }
TreeNodePresenter >> hasChildren: aBlock [

	hasChildren value: aBlock
]

{ #category : #api }
TreeNodePresenter >> hasContentToShow [

	^ hasContentToShow value
]

{ #category : #api }
TreeNodePresenter >> hasContentToShow: aBlock [

	hasContentToShow value: aBlock
]

{ #category : #api }
TreeNodePresenter >> icon [

	^ icon value
]

{ #category : #api }
TreeNodePresenter >> icon: anIcon [

	icon value: anIcon
]

{ #category : #api }
TreeNodePresenter >> iconName: aSelector [

	icon value: (self iconNamed: aSelector)
]

{ #category : #initialization }
TreeNodePresenter >> initialize [

	super initialize.

	children := [ self container childrenFor: self content ] asValueHolder.
	hasChildren := [ self container hasChildrenFor: self content ] asValueHolder.
	content := nil asValueHolder.
	icon := nil asValueHolder.
	container := nil asValueHolder.
	matchTokenFilter := [ :aFilter | false ] asValueHolder.
	parentNode := nil asValueHolder.
	childNodeClass := self class asValueHolder.
	selected := false asValueHolder.
	lastClicked := false asValueHolder.
	mouseDownAction := [:event | ] asValueHolder.
	hasContentToShow := false asValueHolder.
	isExpanded := false asValueHolder.
	
	selected whenChangedDo: [ :aBoolean | self changed: #selected: with: { aBoolean } ].
	isExpanded whenChangedDo: [ :aBoolean | self changed: #isExpanded: with: { aBoolean } ].
]

{ #category : #filtering }
TreeNodePresenter >> isEmpty [
	^ self contents isEmpty
]

{ #category : #api }
TreeNodePresenter >> isExpanded [
	
	^ isExpanded value
]

{ #category : #api }
TreeNodePresenter >> isExpanded: aBoolean [

	isExpanded value: aBoolean
]

{ #category : #api }
TreeNodePresenter >> isPartialMatch [
	| result |

	result := false.
	
	self children value do: [:each | 
		(each selected or: [ each isPartialMatch ]) ifTrue: [ result := true ] ].

	^ result
]

{ #category : #converting }
TreeNodePresenter >> isTreeNodeModel [

	^ true
]

{ #category : #api }
TreeNodePresenter >> lastClicked [
	
	^ lastClicked value
]

{ #category : #api }
TreeNodePresenter >> lastClicked: aBoolean [

	lastClicked value: aBoolean
]

{ #category : #filtering }
TreeNodePresenter >> matchTokenFilter: aFilter [

	^ self matchTokenFilterBlock value: aFilter
]

{ #category : #api }
TreeNodePresenter >> matchTokenFilterBlock [

	^ matchTokenFilter value
]

{ #category : #api }
TreeNodePresenter >> matchTokenFilterBlock: aBlock [

	matchTokenFilter value: aBlock
]

{ #category : #api }
TreeNodePresenter >> mouseDownAction [

	^ mouseDownAction value
]

{ #category : #api }
TreeNodePresenter >> mouseDownAction: aBlock [

	mouseDownAction value: aBlock
]

{ #category : #api }
TreeNodePresenter >> parentNode [

	^ parentNode value
]

{ #category : #api }
TreeNodePresenter >> parentNode: aNode [

	parentNode value: aNode
]

{ #category : #api }
TreeNodePresenter >> selectAllChildren [

	self children value do: [ :e | 
		e selected: true.
		e selectAllChildren ].
]

{ #category : #api }
TreeNodePresenter >> selected [
	
	^ selected value
]

{ #category : #api }
TreeNodePresenter >> selected: aBoolean [

	selected value: aBoolean
]

{ #category : #api }
TreeNodePresenter >> takeHighlight [

	self changed: #takeHighlight with: #()
]

{ #category : #api }
TreeNodePresenter >> unselectAllChildren [

	self children value do: [ :e | 
		e selected: false.
		e unselectAllChildren ].	
]

{ #category : #filtering }
TreeNodePresenter >> updateAccordingTo: aFilter [

	(aFilter keepTreeNode: self) 
		ifFalse: [
			self changed: #updateAccordingTo: with: { aFilter } ]
]

{ #category : #api-events }
TreeNodePresenter >> whenContentChanged: aBlock [

	content whenChangedDo: aBlock

	
]

{ #category : #api-events }
TreeNodePresenter >> whenIsExpandedChanged: aBlock [

	isExpanded whenChangedDo: aBlock

	
]

{ #category : #api-events }
TreeNodePresenter >> whenSelectedChanged: aBlock [

	selected whenChangedDo: aBlock

	
]
