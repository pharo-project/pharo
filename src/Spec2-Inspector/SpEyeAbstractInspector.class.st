"
I am the abstract superclass for all kind of inspectors
"
Class {
	#name : #SpEyeAbstractInspector,
	#superclass : #SpPresenter,
	#traits : 'TSpInspectorActions',
	#classTraits : 'TSpInspectorActions classTrait',
	#instVars : [
		'object',
		'customMenuActions',
		'text'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #spec }
SpEyeAbstractInspector class >> debuggerSpec [
	<spec:#default>
	^ self inspectorSpec
]

{ #category : #spec }
SpEyeAbstractInspector class >> inspectorSpec [
	<spec>
	self subclassResponsibility
]

{ #category : #accessing }
SpEyeAbstractInspector class >> label [
	"Override to return a proper label displayd in the inspector choice"
	^ self name
]

{ #category : #accessing }
SpEyeAbstractInspector class >> taskbarIconName [
	^#smallInspectItIcon
]

{ #category : #'event-handling' }
SpEyeAbstractInspector >> close [
	"Override in sublcasses for special actions"
]

{ #category : #accessing }
SpEyeAbstractInspector >> customMenuActions [
	^ customMenuActions ifNil: [ customMenuActions := [  ] ]
]

{ #category : #accessing }
SpEyeAbstractInspector >> customMenuActions: anObject [
	customMenuActions := anObject
]

{ #category : #'event-handling' }
SpEyeAbstractInspector >> diveInto: anObject [
	self owner ifNotNil: [ :navigator | 
		(navigator respondsTo: #diveInto:)
			ifTrue: [ ^ navigator diveInto: anObject ] ].
		
	self inspectInNewWindow: anObject.
]

{ #category : #actions }
SpEyeAbstractInspector >> diveIntoSelectedObject [
	self selectedObjectDo: [ :anObject | self diveInto: anObject ]
]

{ #category : #testing }
SpEyeAbstractInspector >> hasSelectedObject [
	^ self selectedElement isNotNil
]

{ #category : #initialization }
SpEyeAbstractInspector >> initialize [

	"The inspected object will be stored in the valueObject object to be able to use WhenObjectChanged:"
	
	object := nil asValueHolder.
	super initialize.

]

{ #category : #initialization }
SpEyeAbstractInspector >> initializePresenter [
	object whenChangedDo: [ self objectChanged ].
	self text whenBuiltDo: [ :w | self text behavior: self object class ].
	self initializeShortcuts
]

{ #category : #initialization }
SpEyeAbstractInspector >> initializeShortcuts [
	"override to add custom shortcuts to your model"
]

{ #category : #initialization }
SpEyeAbstractInspector >> initializeWidgets [
]

{ #category : #accessing }
SpEyeAbstractInspector >> inspect: anObject [ 
	self object: anObject.
	
]

{ #category : #menu }
SpEyeAbstractInspector >> inspectionMenu: aMenu [
	| menuModel |
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	menuModel := self newMenu.
	self inspectionSubMenu: menuModel.
	self selectedElement inspectionMenu: menuModel.
	aMenu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	^aMenu
]

{ #category : #menu }
SpEyeAbstractInspector >> inspectionSubMenu: aMenu [

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
SpEyeAbstractInspector >> labelFor: anEyeElement error: error [
	^ 'Error while accessing label of ', anEyeElement accessorCode printString
]

{ #category : #accessing }
SpEyeAbstractInspector >> object [

	^ object value
]

{ #category : #accessing }
SpEyeAbstractInspector >> object: anObject [

	object value: anObject
]

{ #category : #'event-handling' }
SpEyeAbstractInspector >> objectChanged [
	"triggered when the inspected object is changed"
	self subclassResponsibility
]

{ #category : #'event-handling' }
SpEyeAbstractInspector >> ownerChanged [
	self
		customMenuActions: self owner customMenuActions;
		takeKeyboardFocus
]

{ #category : #menu }
SpEyeAbstractInspector >> refreshSubMenu: aMenu [
	"Add a refresh button in the inspector. Not used anymore since there is this loop in initializeInspectorLoop."

	aMenu 
		add: 'Refresh Inspector'
		target: self
		selector: #refresh.	
	aMenu addLine.
	
]

{ #category : #accessing }
SpEyeAbstractInspector >> selectedElement [
	"return the currently selected object wrapper / eye-element"
	^ self subclassResponsibility
]

{ #category : #accessing }
SpEyeAbstractInspector >> selectedElementDo: aBlock [
	self selectedElement ifNotNil: aBlock
]

{ #category : #accessing }
SpEyeAbstractInspector >> selectedObject [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpEyeAbstractInspector >> selectedObjectDo: aBlock [
	self selectedElement ifNotNil: [ :element |
		aBlock value: element value ]
]

{ #category : #initialization }
SpEyeAbstractInspector >> shortCuts [
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
SpEyeAbstractInspector >> text [
	^ text ifNil: [ text := self newCode ]
]

{ #category : #accessing }
SpEyeAbstractInspector >> title [
	^ self object class printString
]

{ #category : #accessing }
SpEyeAbstractInspector >> variants [
	| variants defaultInspectorClass |
	
	variants := OrderedCollection new.
	defaultInspectorClass := self object class inspectorClass.
	
	variants add: defaultInspectorClass. 
	"We only need the basic inspector if we modify something"
	defaultInspectorClass = Object inspectorClass
		ifFalse: [  variants add: SpEyeBasicInspector ].
	variants add: SpEyeTreeInspector.
	variants addAll: self object class additionalInspectorClasses. 
	^ variants
]
