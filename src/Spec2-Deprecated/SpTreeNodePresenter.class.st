"
I represent a node in a tree widget.
I hold an item, an icon and the list of my children.

In addition, I have a filtering mechanism
"
Class {
	#name : #SpTreeNodePresenter,
	#superclass : #SpAbstractWidgetPresenter,
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
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpTreeNodePresenter class >> adapterName [

	^ #TreeNodeAdapter
]

{ #category : #specs }
SpTreeNodePresenter class >> isDeprecated [ 

	^ true
]

{ #category : #api }
SpTreeNodePresenter >> allSelectedItems [
	| result |
	
	result := OrderedCollection new.
	self selected ifTrue: [ result add: self ].
	self children value do: [ :e | 
		result addAll: e allSelectedItems ].
	^ result
]

{ #category : #api }
SpTreeNodePresenter >> allUnselectedItems [
	| result |
	
	result := OrderedCollection new.
	self selected ifFalse: [ result add: self ].
	self children value do: [ :e | 
		result addAll: e allUnselectedItems ].
	^ result
]

{ #category : #api }
SpTreeNodePresenter >> buildChildren [

	contents := self children value collect: [ :each || node | 
		node := (each isKindOf: SpTreeNodePresenter)
			ifTrue: [ each ]
			ifFalse: [  (self childNodeClassFor: each) new
				content: each  ].
		node 
			parentNode: self;
			yourself ].
		
	^ contents
]

{ #category : #api }
SpTreeNodePresenter >> childNodeClass [

	^ childNodeClass value
]

{ #category : #api }
SpTreeNodePresenter >> childNodeClass: aBlock [

	childNodeClass value: aBlock
]

{ #category : #api }
SpTreeNodePresenter >> childNodeClassFor: anItem [

	^ childNodeClass value
]

{ #category : #api }
SpTreeNodePresenter >> children [

	^ children value
]

{ #category : #api }
SpTreeNodePresenter >> children: aBlock [

	children value: aBlock
]

{ #category : #api }
SpTreeNodePresenter >> container [

	^ container value
]

{ #category : #api }
SpTreeNodePresenter >> container: anObject [

	container value: anObject
]

{ #category : #api }
SpTreeNodePresenter >> content [

	^ content value
]

{ #category : #api }
SpTreeNodePresenter >> content: anObject [

	content value: anObject
]

{ #category : #filtering }
SpTreeNodePresenter >> contents [
	^ contents ifNil: [ 
		self buildChildren.
		contents ]
]

{ #category : #api }
SpTreeNodePresenter >> hasChildren [

	^ hasChildren value
]

{ #category : #api }
SpTreeNodePresenter >> hasChildren: aBlock [

	hasChildren value: aBlock
]

{ #category : #api }
SpTreeNodePresenter >> hasContentToShow [

	^ hasContentToShow value
]

{ #category : #api }
SpTreeNodePresenter >> hasContentToShow: aBlock [

	hasContentToShow value: aBlock
]

{ #category : #api }
SpTreeNodePresenter >> icon [

	^ icon value
]

{ #category : #api }
SpTreeNodePresenter >> icon: anIcon [

	icon value: anIcon
]

{ #category : #api }
SpTreeNodePresenter >> iconName: aSelector [

	icon value: (self iconNamed: aSelector)
]

{ #category : #initialization }
SpTreeNodePresenter >> initialize [

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
SpTreeNodePresenter >> isEmpty [
	^ self contents isEmpty
]

{ #category : #api }
SpTreeNodePresenter >> isExpanded [
	
	^ isExpanded value
]

{ #category : #api }
SpTreeNodePresenter >> isExpanded: aBoolean [

	isExpanded value: aBoolean
]

{ #category : #api }
SpTreeNodePresenter >> isPartialMatch [
	| result |

	result := false.
	
	self children value do: [:each | 
		(each selected or: [ each isPartialMatch ]) ifTrue: [ result := true ] ].

	^ result
]

{ #category : #api }
SpTreeNodePresenter >> lastClicked [
	
	^ lastClicked value
]

{ #category : #api }
SpTreeNodePresenter >> lastClicked: aBoolean [

	lastClicked value: aBoolean
]

{ #category : #filtering }
SpTreeNodePresenter >> matchTokenFilter: aFilter [

	^ self matchTokenFilterBlock value: aFilter
]

{ #category : #api }
SpTreeNodePresenter >> matchTokenFilterBlock [

	^ matchTokenFilter value
]

{ #category : #api }
SpTreeNodePresenter >> matchTokenFilterBlock: aBlock [

	matchTokenFilter value: aBlock
]

{ #category : #api }
SpTreeNodePresenter >> mouseDownAction [

	^ mouseDownAction value
]

{ #category : #api }
SpTreeNodePresenter >> mouseDownAction: aBlock [

	mouseDownAction value: aBlock
]

{ #category : #api }
SpTreeNodePresenter >> parentNode [

	^ parentNode value
]

{ #category : #api }
SpTreeNodePresenter >> parentNode: aNode [

	parentNode value: aNode
]

{ #category : #api }
SpTreeNodePresenter >> selectAllChildren [

	self children value do: [ :e | 
		e selected: true.
		e selectAllChildren ].
]

{ #category : #api }
SpTreeNodePresenter >> selected [
	
	^ selected value
]

{ #category : #api }
SpTreeNodePresenter >> selected: aBoolean [

	selected value: aBoolean
]

{ #category : #api }
SpTreeNodePresenter >> takeHighlight [

	self changed: #takeHighlight with: #()
]

{ #category : #api }
SpTreeNodePresenter >> unselectAllChildren [

	self children value do: [ :e | 
		e selected: false.
		e unselectAllChildren ].	
]

{ #category : #filtering }
SpTreeNodePresenter >> updateAccordingTo: aFilter [

	(aFilter keepTreeNode: self) 
		ifFalse: [
			self changed: #updateAccordingTo: with: { aFilter } ]
]

{ #category : #'as yet unclassified' }
SpTreeNodePresenter >> whenContentChanged: aBlock [
	self deprecated: 'Use #whenContentChangedDo: instead.' transformWith: '`@receiver whenContentChanged: `@statements' -> '`@receiver whenContentChangedDo: `@statements'.
	self whenContentChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreeNodePresenter >> whenContentChangedDo: aBlock [

	content whenChangedDo: aBlock

	
]

{ #category : #'as yet unclassified' }
SpTreeNodePresenter >> whenIsExpandedChanged: aBlock [
	self deprecated: 'Use #whenIsExpandedChangedDo: instead.' transformWith: '`@receiver whenIsExpandedChanged: `@statements' -> '`@receiver whenIsExpandedChangedDo: `@statements'.
	self whenIsExpandedChangedDo: aBlock

	
]

{ #category : #'api-events' }
SpTreeNodePresenter >> whenIsExpandedChangedDo: aBlock [

	isExpanded whenChangedDo: aBlock

	
]

{ #category : #'as yet unclassified' }
SpTreeNodePresenter >> whenSelectedChanged: aBlock [
	self deprecated: 'Use #whenSelectedChangedDo: instead.' transformWith: '`@receiver whenSelectedChanged: `@statements' -> '`@receiver whenSelectedChangedDo: `@statements'.
	self whenSelectedChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreeNodePresenter >> whenSelectedChangedDo: aBlock [

	selected whenChangedDo: aBlock

	
]
