"
This a wrapper around inspectors for diving. It is used in the case where the inspector is dynamically replaced by another inspector. For example, EyeCollectionInspector would be replaced by EyeIntegerInspector.
"
Class {
	#name : #EyeInspectorNavigator,
	#superclass : #SpPresenter,
	#instVars : [
		'#inspector => SpObservableSlot',
		'#history',
		'#canBeRefreshed',
		'#toolbar'
	],
	#category : #'Spec2-Inspector-Widgets'
}

{ #category : #specs }
EyeInspectorNavigator class >> debuggerSpec [
	<spec: #default>
	^ self layoutForSpec: #debuggerSpec
]

{ #category : #'instance creation' }
EyeInspectorNavigator class >> inspect: anObject [
	^ self openInspector: anObject inspector
]

{ #category : #specs }
EyeInspectorNavigator class >> inspectorSpec [
	<spec>
	^ self layoutForSpec: #inspectorSpec
]

{ #category : #specs }
EyeInspectorNavigator class >> layoutForSpec: spec [
	^ SpBoxLayout newVertical
		add: #toolbar withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		add: #inspector withConstraints: [ :constraints | constraints spec: spec ];
		yourself
]

{ #category : #'instance creation' }
EyeInspectorNavigator class >> openExplorer: inspector [

	^ (self openInspector: inspector) useTreeInspector
]

{ #category : #'instance creation' }
EyeInspectorNavigator class >> openInspector: inspector [

	^ self new
		inspector: inspector;
		openWithSpec: #inspectorSpec;
		yourself
]

{ #category : #accessing }
EyeInspectorNavigator >> basicInspector: anEyeInspector [
	inspector ifNotNil: [ :inspect | inspect owner: nil ].
	inspector := anEyeInspector.

	anEyeInspector
		owner: self;
		ownerChanged
]

{ #category : #accessing }
EyeInspectorNavigator >> canBeRefreshed [
	^ canBeRefreshed
]

{ #category : #accessing }
EyeInspectorNavigator >> canBeRefreshed: anObject [
	canBeRefreshed := anObject
]

{ #category : #menu }
EyeInspectorNavigator >> customMenuActions [
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
EyeInspectorNavigator >> defaultWindowPresenterClass [
	^ SpTickingWindowPresenter
]

{ #category : #actions }
EyeInspectorNavigator >> diveInto: anObject [
	self inspect: anObject.
]

{ #category : #actions }
EyeInspectorNavigator >> diveIntoSelectedObject [
	self inspector diveIntoSelectedObject
]

{ #category : #accessing }
EyeInspectorNavigator >> history [
	^ history
]

{ #category : #initialization }
EyeInspectorNavigator >> initialExtent [
	^ self inspector initialExtent
	
]

{ #category : #initialization }
EyeInspectorNavigator >> initialize [

	super initialize.
	
	history := NavigationHistory new.
	
	self initializeShortcuts.
	self property: #inspector whenChangedDo: [ :new :old | 
		self updateInspectorFrom: old ].
	self announcer
		when: SpWidgetBuilt send: #setRefreshedToTrue to: self.
	
	canBeRefreshed := false.
	self whenWindowChanged: [ :w |
		w whenClosedDo: [ self inspector close ] ].
]

{ #category : #initialization }
EyeInspectorNavigator >> initializeShortcuts [
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
EyeInspectorNavigator >> initializeWidgets [
	toolbar := self instantiate: EyeInspectorToolBar
]

{ #category : #actions }
EyeInspectorNavigator >> inspect: anObject [
	| oldInspectorType |
	oldInspectorType := self inspectorType.
	self 
		inspector: (EyeInspector inspector: anObject);
		inspectorType: oldInspectorType.

]

{ #category : #history }
EyeInspectorNavigator >> inspectHistoryItem: historyItem [
	"self inspect: historyItem key .
	self selectedIndex: historyItem value."
	self inspector: historyItem key
]

{ #category : #actions }
EyeInspectorNavigator >> inspectNext [
	history hasNext
		ifFalse: [ ^ self ].
	self inspectHistoryItem: history next
]

{ #category : #actions }
EyeInspectorNavigator >> inspectPrevious [
	history hasPrevious
		ifFalse: [ ^ self ].
	self inspectHistoryItem: history previous
]

{ #category : #accessing }
EyeInspectorNavigator >> inspector [
	^ inspector
]

{ #category : #accessing }
EyeInspectorNavigator >> inspector: anEyeInspector [
	self basicInspector: anEyeInspector.
	self pushHistory.
]

{ #category : #accessing }
EyeInspectorNavigator >> inspectorType [
	^ self inspector class
]

{ #category : #accessing }
EyeInspectorNavigator >> inspectorType: anInspectorClass [
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
EyeInspectorNavigator >> inspectorTypes [
	^ self inspector variants
]

{ #category : #accessing }
EyeInspectorNavigator >> nextInspectorType [
	| index types |
	types := self inspectorTypes.
	index := types 
		indexOf: self inspectorType
		ifAbsent: [ ^ types first ].
	^types atWrap: index + 1.
]

{ #category : #accessing }
EyeInspectorNavigator >> object [
	^ self inspector object
]

{ #category : #accessing }
EyeInspectorNavigator >> previousInspectorType [
	| index types |
	types := self inspectorTypes.
	index := types 
		indexOf: self inspectorType
		ifAbsent: [ ^ types first ].
	^types atWrap: index - 1.
]

{ #category : #history }
EyeInspectorNavigator >> pushHistory [
	history add: self inspector.
	self toolbar update.
]

{ #category : #accessing }
EyeInspectorNavigator >> selectedIndex [
	^ self inspector selectedIndex
]

{ #category : #accessing }
EyeInspectorNavigator >> selectedIndex: anInteger [
	self inspector selectedIndex: anInteger
]

{ #category : #accessing }
EyeInspectorNavigator >> selectedObject [
	^ self inspector selectedObject
]

{ #category : #private }
EyeInspectorNavigator >> setRefreshedToTrue [
	"introduced because we should not use weak announcer with on:do:."
	
	canBeRefreshed := true
]

{ #category : #stepping }
EyeInspectorNavigator >> step [
	self inspector step.
]

{ #category : #stepping }
EyeInspectorNavigator >> stepTime [
	^ 1500
]

{ #category : #updating }
EyeInspectorNavigator >> takeKeyboardFocus [
	^ self inspector takeKeyboardFocus
]

{ #category : #accessing }
EyeInspectorNavigator >> taskbarIcon [
	^ self inspector 
		ifNil: [ super taskbarIcon ]
		ifNotNil: [ :anInspector | anInspector taskbarIcon ]
]

{ #category : #accessing }
EyeInspectorNavigator >> title [
	^ self inspector title
]

{ #category : #accessing }
EyeInspectorNavigator >> toolbar [
	^ toolbar
]

{ #category : #accessing }
EyeInspectorNavigator >> toolbar: anObject [ 
	toolbar := anObject 
]

{ #category : #updating }
EyeInspectorNavigator >> updateInspectorFrom: oldInspector [

	self updateTitle.
	self inspector ifNil: [ ^ self ].

	self canBeRefreshed ifFalse: [ ^ self ].

	self needRebuild: false.

	self buildWithSpec: oldInspector spec selector.
	
	oldInspector close. "kill the update process"
]

{ #category : #actions }
EyeInspectorNavigator >> useBasicInspector [
	self inspectorType: EyeBasicInspector
]

{ #category : #actions }
EyeInspectorNavigator >> useDefaultInspector [
	self inspectorType: self object class inspectorClass
]

{ #category : #actions }
EyeInspectorNavigator >> useInspectorAt: index [
	"choose an inspector at the given index from the given inspectorTypes.
	If the index is out of bounds, simply crop it to the neares value"
	| types |
	types := self inspectorTypes.
	self inspectorType: (types at: (index min: types size max: 1))
]

{ #category : #actions }
EyeInspectorNavigator >> useNextInspectorType [
	self inspectorType: self nextInspectorType
]

{ #category : #actions }
EyeInspectorNavigator >> usePreviousInspectorType [
	self inspectorType: self previousInspectorType
]

{ #category : #actions }
EyeInspectorNavigator >> useTreeInspector [
	self inspectorType: EyeTreeInspector
]
