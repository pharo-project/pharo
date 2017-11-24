"
I am the abstract superclass for all kind of inspectors
"
Class {
	#name : #EyeAbstractInspector,
	#superclass : #ComposablePresenter,
	#traits : 'TInspectorActions',
	#classTraits : 'TInspectorActions classTrait',
	#instVars : [
		'object',
		'customMenuActions',
		'text'
	],
	#category : #'Spec-Inspector'
}

{ #category : #spec }
EyeAbstractInspector class >> debuggerSpec [
	<spec:#default>
	^ self inspectorSpec
]

{ #category : #spec }
EyeAbstractInspector class >> inspectorSpec [
	<spec>
	self subclassResponsibility
]

{ #category : #accessing }
EyeAbstractInspector class >> label [
	"Override to return a proper label displayd in the inspector choice"
	^ self name
]

{ #category : #accessing }
EyeAbstractInspector class >> taskbarIconName [
	^#smallInspectItIcon
]

{ #category : #'event-handling' }
EyeAbstractInspector >> close [
	"Override in sublcasses for special actions"
]

{ #category : #accessing }
EyeAbstractInspector >> customMenuActions [
	^ customMenuActions ifNil: [ customMenuActions := [  ] ]
]

{ #category : #accessing }
EyeAbstractInspector >> customMenuActions: anObject [
	customMenuActions := anObject
]

{ #category : #'event-handling' }
EyeAbstractInspector >> diveInto: anObject [
	self owner ifNotNil: [ :navigator | 
		(navigator respondsTo: #diveInto:)
			ifTrue: [ ^ navigator diveInto: anObject ] ].
		
	self inspectInNewWindow: anObject.
]

{ #category : #actions }
EyeAbstractInspector >> diveIntoSelectedObject [
	self selectedObjectDo: [ :anObject | self diveInto: anObject ]
]

{ #category : #testing }
EyeAbstractInspector >> hasSelectedObject [
	^ self selectedElement isNotNil
]

{ #category : #initialization }
EyeAbstractInspector >> initialize [

	"The inspected object will be stored in the valueObject object to be able to use WhenObjectChanged:"
	
	object := nil asValueHolder.
	super initialize.

]

{ #category : #initialization }
EyeAbstractInspector >> initializePresenter [
	object whenChangedDo: [ self objectChanged ].
	self text
		whenBuiltDo: [ :w | w widget editingMode classOrMetaClass: self object class ].
	self initializeShortcuts
]

{ #category : #initialization }
EyeAbstractInspector >> initializeShortcuts [
	"override to add custom shortcuts to your model"
]

{ #category : #initialization }
EyeAbstractInspector >> initializeWidgets [
]

{ #category : #accessing }
EyeAbstractInspector >> inspect: anObject [ 
	self object: anObject.
	
]

{ #category : #menu }
EyeAbstractInspector >> inspectionMenu: aMenu [
	| menuModel |
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	menuModel := MenuPresenter new.
	self inspectionSubMenu: menuModel.
	self selectedElement inspectionMenu: menuModel.
	aMenu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	^aMenu
]

{ #category : #menu }
EyeAbstractInspector >> inspectionSubMenu: aMenu [

	aMenu addGroup: [:aGroup |
		aGroup addItem: [ :item |
			item
				name: 'Inspect' translated;
				action: [ self inspectSelectedObjectInNewWindow ];
				shortcut: $i meta ].
		aGroup addItem: [ :item |
			item
				name: 'Explore' translated;
				action: [ self exploreSelectedObject ];
				shortcut: $i meta shift ].

	self customMenuActions cull: aMenu cull: aGroup.
			
	]
]

{ #category : #api }
EyeAbstractInspector >> labelFor: anEyeElement error: error [
	^ 'Error while accessing label of ', anEyeElement accessorCode printString
]

{ #category : #accessing }
EyeAbstractInspector >> object [

	^ object value
]

{ #category : #accessing }
EyeAbstractInspector >> object: anObject [

	object value: anObject
]

{ #category : #'event-handling' }
EyeAbstractInspector >> objectChanged [
	"triggered when the inspected object is changed"
	self subclassResponsibility
]

{ #category : #'event-handling' }
EyeAbstractInspector >> ownerChanged [
	self
		customMenuActions: self owner customMenuActions;
		takeKeyboardFocus
]

{ #category : #menu }
EyeAbstractInspector >> refreshSubMenu: aMenu [
	"Add a refresh button in the inspector. Not used anymore since there is this loop in initializeInspectorLoop."

	aMenu 
		add: 'Refresh Inspector'
		target: self
		selector: #refresh.	
	aMenu addLine.
	
]

{ #category : #accessing }
EyeAbstractInspector >> selectedElement [
	"return the currently selected object wrapper / eye-element"
	^ self subclassResponsibility
]

{ #category : #accessing }
EyeAbstractInspector >> selectedElementDo: aBlock [
	self selectedElement ifNotNil: aBlock
]

{ #category : #accessing }
EyeAbstractInspector >> selectedObject [
	^ self subclassResponsibility
]

{ #category : #accessing }
EyeAbstractInspector >> selectedObjectDo: aBlock [
	self selectedElement ifNotNil: [ :element |
		aBlock value: element value ]
]

{ #category : #initialization }
EyeAbstractInspector >> shortCuts [
	"Answers a dictionary of the shortcuts usable in EyeInspector"

	|shortCuts|

	shortCuts := Dictionary new.
	
	shortCuts at: $d meta put: [ self diveIntoSelectedObject ].
	
	shortCuts at: $i meta put: [ self inspectSelectedObjectInNewWindow ].
	shortCuts at: $i meta shift put: [ self exploreSelectedObject ].
	
	shortCuts at: $b meta put: [ self browseSelectedObject ].
	shortCuts at: $h meta put: [ self browseSelectedObjectClassHierarchy ].
	
	^shortCuts
]

{ #category : #accessing }
EyeAbstractInspector >> text [
	^ text ifNil: [ 
		text := self newCode.
		text aboutToStyle: true. ]
]

{ #category : #accessing }
EyeAbstractInspector >> title [
	^ self object class printString
]

{ #category : #accessing }
EyeAbstractInspector >> variants [
	| variants defaultInspectorClass |
	
	variants := OrderedCollection new.
	defaultInspectorClass := self object class inspectorClass.
	
	variants add: defaultInspectorClass. 
	"We only need the basic inspector if we modify something"
	defaultInspectorClass = Object inspectorClass
		ifFalse: [  variants add: EyeBasicInspector ].
	variants add: EyeTreeInspector.
	variants addAll: self object class additionalInspectorClasses. 
	^ variants
]
