"
I am an inspector of objects. 
I offer multiple views using a miller list as a navigation. 
One particular feature is that you can use the evaluator tab to enter code, and evaluating it results in opening another pane to the right.

The object finder asks dynamically the object for the actual presentations that are displayed in each pane.

Example:
self openOn: Smalltalk.

Register it as a replacement for inspector:
self registerToolsOn: Smalltalk tools.
"
Class {
	#name : #StInspector,
	#superclass : #StPresenter,
	#instVars : [
		'millerList',
		'model',
		'lastPageSelectedTabName',
		'withHeaderBar'
	],
	#classVars : [
		'IndexableDisplayLimit',
		'PreferredExtent'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #accessing }
StInspector class >> defaultSpacing [

	^ 3
]

{ #category : #specs }
StInspector class >> defaultSpec [

	^ SpBoxLayout newVertical
		add: #millerList;
		yourself
]

{ #category : #'instance creation' }
StInspector class >> explore: anObject [
 
	self openOn: anObject.
	^ anObject
]

{ #category : #settings }
StInspector class >> indexableDisplayLimit [

	^ IndexableDisplayLimit ifNil: [ 5000 ]
]

{ #category : #settings }
StInspector class >> indexableDisplayLimit: aNumber [

	IndexableDisplayLimit := aNumber
]

{ #category : #'instance creation' }
StInspector class >> inspect: anObject [

	^ self openOn: anObject
]

{ #category : #'instance creation' }
StInspector class >> inspect: anObject label: aString [

	(self openOn: anObject) title: aString.
	^ anObject

]

{ #category : #'instance creation' }
StInspector class >> inspector: anObject [
	
	self 
		deprecated: 'Use #inspect: instead.'
		transformWith: '`@receiver inspector: `@statements1'
				-> '`@receiver inspect: `@statements1'.

	^ self openOn: anObject
]

{ #category : #'instance creation' }
StInspector class >> onObject: anObject [

	^ self on: (StInspectorModel on: anObject)
]

{ #category : #'instance creation' }
StInspector class >> openOn: anObject [

	^ (self onObject: anObject) openWithSpec
]

{ #category : #accessing }
StInspector class >> preferredExtent [
	
	^ PreferredExtent ifNil: [ PreferredExtent := 600@400 ]
]

{ #category : #accessing }
StInspector class >> preferredExtent: aPoint [
	
	PreferredExtent := aPoint
]

{ #category : #'tools registry' }
StInspector class >> register [
	<script>

	self registerToolsOn: Smalltalk tools
]

{ #category : #'tools registry' }
StInspector class >> registerToolsOn: registry [
	"self registerToolsOn: Smalltalk tools " 
	registry register: self as: #inspector
]

{ #category : #transmission }
StInspector >> defaultInputPort [

	^ self inputModelPort
]

{ #category : #'api-focus' }
StInspector >> defaultKeyboardFocus [

	^ millerList presenters 
		ifNotEmpty: [ :list | list first ] 
		ifEmpty: [ self ]
]

{ #category : #initialization }
StInspector >> initializePresenters [

	millerList := self instantiate: SpMillerColumnPresenter.
	millerList 
		spacing: self spacing;
		presenterBlock: [ :aModel | self newInspectorFor: aModel ];
		setRootModel: self model;
		whenColumnsWillChangeDo: [ :insertIndex | self keepLastPageSelectedTabName ]
]

{ #category : #initialization }
StInspector >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.
	aWindowPresenter 
		title: self model windowTitle;
		initialExtent: self class preferredExtent;
		whenOpenedDo: [ 
			self startStepping.
			self takeKeyboardFocus ];
		whenClosedDo: [ self stopStepping ];
		whenResizingDo: [ :ann | self class preferredExtent: ann newSize ]
]

{ #category : #transmission }
StInspector >> inputModelPort [

	^ SpModelPort newPresenter: self
]

{ #category : #private }
StInspector >> keepLastPageSelectedTabName [
	
	lastPageSelectedTabName := millerList pages 
		ifNotEmpty: [ :pages | pages last selectedPageName ]
		ifEmpty: [ nil ]
]

{ #category : #accessing }
StInspector >> model [

	^ model
]

{ #category : #'accessing model' }
StInspector >> model: anInspectorModel [

	model := anInspectorModel.
	self updateTitle.
	self updateList
]

{ #category : #'private factory' }
StInspector >> newInspectorFor: aModel [
	| newInspector |
	newInspector := self
		instantiate: aModel defaultObjectInspectorClass
		on: (self toInspectorModel: aModel).
	millerList pages
		ifNotEmpty: [ :pages | 
			newInspector
				trySelectAnyPageNamed:
					{lastPageSelectedTabName.
					pages last selectedPageName} ].
	^ newInspector
]

{ #category : #private }
StInspector >> setModel: aModel [

	self model: aModel
]

{ #category : #'accessing model' }
StInspector >> setModelBeforeInitialization: anInspectorModel [

	model := anInspectorModel
]

{ #category : #accessing }
StInspector >> spacing [
	
	^ self class defaultSpacing
]

{ #category : #'private stepping' }
StInspector >> startStepping [
	
	StInspectorRefreshService uniqueInstance register: self
]

{ #category : #'private stepping' }
StInspector >> step [ 

	millerList pages
		do: [ :each | each step ]
]

{ #category : #'private stepping' }
StInspector >> stopStepping [

	StInspectorRefreshService uniqueInstance unregister: self
]

{ #category : #private }
StInspector >> toInspectorModel: aModel [

	self flag: #HACK. "I do not like to use this, but I really do not want to add a double 
	dispatch to solve this problem :("
	(aModel isKindOf: StInspectorModel) ifTrue: [ ^ aModel ].
	(aModel isKindOf: StInspectorNode) ifTrue: [ ^ StInspectorModel on: aModel rawValue ].

	^ StInspectorModel on: aModel
]

{ #category : #'private updating' }
StInspector >> updateList [

	millerList 
		resetTo: 0;
		setRootModel: self model
]

{ #category : #'private updating' }
StInspector >> updateTitle [
		
	self isRoot ifFalse: [ ^ self ].
	self withWindowDo: [ :window | 
		window title: self title ].
]

{ #category : #accessing }
StInspector >> withHeaderBar [
	 ^withHeaderBar ifNil:[withHeaderBar := true]
]

{ #category : #accessing }
StInspector >> withHeaderBar: bool [
	withHeaderBar := bool
]
