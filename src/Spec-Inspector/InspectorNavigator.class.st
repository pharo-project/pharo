"
This a wrapper around inspectors for diving. It is used in the case where the inspector is dynamically replaced by another inspector. For example, EyeCollectionInspector would be replaced by EyeIntegerInspector.
"
Class {
	#name : #InspectorNavigator,
	#superclass : #ComposablePresenter,
	#instVars : [
		'inspector',
		'history',
		'canBeRefreshed',
		'listOfOpenedInspectors',
		'toolbar'
	],
	#category : #'Spec-Inspector'
}

{ #category : #specs }
InspectorNavigator class >> debuggerSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newRow: #toolbar height: self toolbarHeight;
		add: #inspector withSpec: #debuggerSpec top: self toolbarHeight;
		yourself
]

{ #category : #'instance creation' }
InspectorNavigator class >> inspect: anObject [
	^ self openInspector: anObject inspector
]

{ #category : #specs }
InspectorNavigator class >> inspectorSpec [
	<spec>
	
	^ SpecLayout composed
		newColumn: [ :c | 
			c 
				add: #toolbar height: self toolbarHeight;
				add: #inspector withSpec: #inspectorSpec ];
		yourself
]

{ #category : #'instance creation' }
InspectorNavigator class >> openExplorer: inspector [

	^ (self openInspector: inspector) useTreeInspector
]

{ #category : #'instance creation' }
InspectorNavigator class >> openInspector: inspector [

	^ self new
		inspector: inspector;
		openWithSpec: #inspectorSpec;
		yourself
]

{ #category : #accessing }
InspectorNavigator >> basicInspector: anEyeInspector [
	inspector value 
		ifNotNil: [ :inspect | 
			inspect owner: nil ].
	inspector value: anEyeInspector.
	
	anEyeInspector 
		owner: self;
		ownerChanged.
]

{ #category : #accessing }
InspectorNavigator >> canBeRefreshed [
	^ canBeRefreshed
]

{ #category : #accessing }
InspectorNavigator >> canBeRefreshed: anObject [
	canBeRefreshed := anObject
]

{ #category : #menu }
InspectorNavigator >> customMenuActions [
	^ [:aMenu :currentGroup |
		currentGroup addItem: [ :item |
			item
				name: 'Dive Into' translated;
				action: [ self diveIntoSelectedObject  ];
				shortcut: $d meta ].
	
		(history hasPrevious or: [ history hasNext ])
			ifTrue: [ 
				aMenu addGroup: [ :aGroup |
					history hasPrevious ifTrue: [ 
						aGroup addItem: [ :item |
							item
								name: 'Back' translated;
								action: [ self inspectPrevious ];
								shortcut: $[ meta ] ].
							
					history hasNext ifTrue: [ 
						aGroup addItem: [ :item |
							item
								name: 'Forward' translated;
								action: [ self inspectNext ];
								shortcut: $] meta  ] ] ] ] ]
]

{ #category : #accessing }
InspectorNavigator >> defaultWindowModelClass [
	Stdio stdout wantsLineEndConversion: true; converter.
	Stdio stdout << self class name ; cr.
	^ TickingWindowPresenter
]

{ #category : #actions }
InspectorNavigator >> diveInto: anObject [
	self inspect: anObject.
]

{ #category : #actions }
InspectorNavigator >> diveIntoSelectedObject [
	self inspector diveIntoSelectedObject
]

{ #category : #accessing }
InspectorNavigator >> history [
	^ history
]

{ #category : #initialization }
InspectorNavigator >> initialExtent [
	^ self inspector initialExtent
	
]

{ #category : #initialization }
InspectorNavigator >> initialize [

	super initialize.
	
	history := NavigationHistory new.
	inspector := nil asValueHolder.
	
	self initializeShortcuts.
	inspector whenChangedDo: [ :new :old | 
		self updateInspectorFrom: old ].
	self announcer
		when: WidgetBuilt send: #setRefreshedToTrue to: self.
	
	canBeRefreshed := false.
	self whenWindowChanged: [ :w |
		w whenClosedDo: [ self inspector close ] ].
]

{ #category : #initialization }
InspectorNavigator >> initializeShortcuts [
	self bindKeyCombination: $[ meta toAction: [ self inspectPrevious ].
	self bindKeyCombination: $] meta toAction: [ self inspectNext ].

	self bindKeyCombination: $[ meta shift toAction: [ self usePreviousInspectorType ].
	self bindKeyCombination: $] meta shift toAction: [ self useNextInspectorType ].

	self bindKeyCombination: $0 meta toAction: [ self useBasicInspector ].
	self bindKeyCombination: $0 meta shift toAction: [ self useDefaultInspector ].
	
	"code duplication? yes, but still easier than capturing arguments ;)"
	self bindKeyCombination: $1 meta toAction: [ self useInspectorAt: 1 ].
	self bindKeyCombination: $2 meta toAction: [ self useInspectorAt: 2 ].
	self bindKeyCombination: $3 meta toAction: [ self useInspectorAt: 3 ].
	self bindKeyCombination: $4 meta toAction: [ self useInspectorAt: 4 ].
	self bindKeyCombination: $5 meta toAction: [ self useInspectorAt: 5 ].
	self bindKeyCombination: $6 meta toAction: [ self useInspectorAt: 6 ].
	self bindKeyCombination: $7 meta toAction: [ self useInspectorAt: 7 ].
	self bindKeyCombination: $8 meta toAction: [ self useInspectorAt: 8 ].
	self bindKeyCombination: $9 meta toAction: [ self useInspectorAt: 9 ].
]

{ #category : #initialization }
InspectorNavigator >> initializeWidgets [

	
	toolbar := self instantiate: EyeInspectorToolBar.
]

{ #category : #actions }
InspectorNavigator >> inspect: anObject [
	| oldInspectorType |
	oldInspectorType := self inspectorType.
	self 
		inspector: (EyeInspector inspector: anObject);
		inspectorType: oldInspectorType.

]

{ #category : #history }
InspectorNavigator >> inspectHistoryItem: historyItem [
	"self inspect: historyItem key .
	self selectedIndex: historyItem value."
	self inspector: historyItem key
]

{ #category : #actions }
InspectorNavigator >> inspectNext [
	history hasNext
		ifFalse: [ ^ self ].
	self inspectHistoryItem: history next
]

{ #category : #actions }
InspectorNavigator >> inspectPrevious [
	history hasPrevious
		ifFalse: [ ^ self ].
	self inspectHistoryItem: history previous
]

{ #category : #accessing }
InspectorNavigator >> inspector [
	^ inspector value
]

{ #category : #accessing }
InspectorNavigator >> inspector: anEyeInspector [
	self basicInspector: anEyeInspector.
	self pushHistory.
]

{ #category : #accessing }
InspectorNavigator >> inspectorType [
	^ self inspector class
]

{ #category : #accessing }
InspectorNavigator >> inspectorType: anInspectorClass [
	"Use a new inspector type. Instantiate the given inspector class and replace the current view"
	| oldScriptAreaTextModel |
	
	oldScriptAreaTextModel := self inspector text.
	
	anInspectorClass = self inspector class
		ifTrue: [  ^ self ].
	anInspectorClass isNil
		ifTrue: [ ^ self ].
		
	(self inspectorTypes includes: anInspectorClass)
		ifFalse: [ ^ self ].
		
	self basicInspector: (anInspectorClass new 
		inspect: self object;
		yourself).

	self inspector text 
		text: oldScriptAreaTextModel getText;
		setSelection: oldScriptAreaTextModel getSelection.

	history replaceCurrentWith: self inspector.
	self toolbar update.
]

{ #category : #accessing }
InspectorNavigator >> inspectorTypes [
	^ self inspector variants
]

{ #category : #accessing }
InspectorNavigator >> nextInspectorType [
	| index types |
	types := self inspectorTypes.
	index := types 
		indexOf: self inspectorType
		ifAbsent: [ ^ types first ].
	^types atWrap: index + 1.
]

{ #category : #accessing }
InspectorNavigator >> object [
	^ self inspector object
]

{ #category : #accessing }
InspectorNavigator >> previousInspectorType [
	| index types |
	types := self inspectorTypes.
	index := types 
		indexOf: self inspectorType
		ifAbsent: [ ^ types first ].
	^types atWrap: index - 1.
]

{ #category : #history }
InspectorNavigator >> pushHistory [
	history add: self inspector.
	self toolbar update.
]

{ #category : #accessing }
InspectorNavigator >> selectedIndex [
	^ self inspector selectedIndex
]

{ #category : #accessing }
InspectorNavigator >> selectedIndex: anInteger [
	self inspector selectedIndex: anInteger
]

{ #category : #accessing }
InspectorNavigator >> selectedObject [
	^ self inspector selectedObject
]

{ #category : #private }
InspectorNavigator >> setRefreshedToTrue [
	"introduced because we should not use weak announcer with on:do:."
	
	canBeRefreshed := true
]

{ #category : #initialization }
InspectorNavigator >> step [
	self inspector step.
]

{ #category : #initialization }
InspectorNavigator >> stepTime [
	^ 1500
]

{ #category : #updating }
InspectorNavigator >> takeKeyboardFocus [
	^ self inspector takeKeyboardFocus
]

{ #category : #accessing }
InspectorNavigator >> taskbarIcon [
	^ self inspector 
		ifNil: [ super taskbarIcon ]
		ifNotNil: [ :anInspector | anInspector taskbarIcon ]
]

{ #category : #accessing }
InspectorNavigator >> title [
	^ self inspector title
]

{ #category : #accessing }
InspectorNavigator >> toolbar [
	^ toolbar
]

{ #category : #accessing }
InspectorNavigator >> toolbar: anObject [ 
	toolbar := anObject 
]

{ #category : #updating }
InspectorNavigator >> updateInspectorFrom: oldInspector [

	self updateTitle.
	self inspector ifNil: [ ^ self ].

	self canBeRefreshed ifFalse: [ ^ self ].

	self needRebuild: false.

	self buildWithSpec: oldInspector spec selector.
	
	oldInspector close. "kill the update process"
]

{ #category : #actions }
InspectorNavigator >> useBasicInspector [
	self inspectorType: EyeBasicInspector
]

{ #category : #actions }
InspectorNavigator >> useDefaultInspector [
	self inspectorType: self object class inspectorClass
]

{ #category : #actions }
InspectorNavigator >> useInspectorAt: index [
	"choose an inspector at the given index from the given inspectorTypes.
	If the index is out of bounds, simply crop it to the neares value"
	| types |
	types := self inspectorTypes.
	self inspectorType: (types at: (index min: types size max: 1))
]

{ #category : #actions }
InspectorNavigator >> useNextInspectorType [
	self inspectorType: self nextInspectorType
]

{ #category : #actions }
InspectorNavigator >> usePreviousInspectorType [
	self inspectorType: self previousInspectorType
]

{ #category : #actions }
InspectorNavigator >> useTreeInspector [
	self inspectorType: EyeTreeInspector
]
