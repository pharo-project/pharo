"
This a wrapper around inspectors for diving. It is used in the case where the inspector is dynamically replaced by another inspector. For example, EyeCollectionInspector would be replaced by EyeIntegerInspector.
"
Class {
	#name : #SpInspectorNavigator,
	#superclass : #SpPresenter,
	#instVars : [
		'inspector',
		'history',
		'canBeRefreshed',
		'toolbar'
	],
	#category : #'Spec2-Inspector-Widgets'
}

{ #category : #specs }
SpInspectorNavigator class >> debuggerSpec [
	<spec: #default>
	
	^ SpLayout composed
		newRow: #toolbar height: self toolbarHeight;
		add: #inspector withSpec: #debuggerSpec top: self toolbarHeight;
		yourself
]

{ #category : #'instance creation' }
SpInspectorNavigator class >> inspect: anObject [
	^ self openInspector: anObject inspector
]

{ #category : #specs }
SpInspectorNavigator class >> inspectorSpec [
	<spec>
	
	^ SpLayout composed
		newColumn: [ :c | 
			c 
				add: #toolbar height: self toolbarHeight;
				add: #inspector withSpec: #inspectorSpec ];
		yourself
]

{ #category : #'instance creation' }
SpInspectorNavigator class >> openExplorer: inspector [

	^ (self openInspector: inspector) useTreeInspector
]

{ #category : #'instance creation' }
SpInspectorNavigator class >> openInspector: inspector [

	^ self new
		inspector: inspector;
		openWithSpec: #inspectorSpec;
		yourself
]

{ #category : #accessing }
SpInspectorNavigator >> basicInspector: anEyeInspector [
	inspector value 
		ifNotNil: [ :inspect | 
			inspect owner: nil ].
	inspector value: anEyeInspector.
	
	anEyeInspector 
		owner: self;
		ownerChanged.
]

{ #category : #accessing }
SpInspectorNavigator >> canBeRefreshed [
	^ canBeRefreshed
]

{ #category : #accessing }
SpInspectorNavigator >> canBeRefreshed: anObject [
	canBeRefreshed := anObject
]

{ #category : #menu }
SpInspectorNavigator >> customMenuActions [
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
SpInspectorNavigator >> defaultWindowPresenterClass [
	^ SpTickingWindowPresenter
]

{ #category : #actions }
SpInspectorNavigator >> diveInto: anObject [
	self inspect: anObject.
]

{ #category : #actions }
SpInspectorNavigator >> diveIntoSelectedObject [
	self inspector diveIntoSelectedObject
]

{ #category : #accessing }
SpInspectorNavigator >> history [
	^ history
]

{ #category : #initialization }
SpInspectorNavigator >> initialExtent [
	^ self inspector initialExtent
	
]

{ #category : #initialization }
SpInspectorNavigator >> initialize [

	super initialize.
	
	history := NavigationHistory new.
	inspector := nil asValueHolder.
	
	self initializeShortcuts.
	inspector whenChangedDo: [ :new :old | 
		self updateInspectorFrom: old ].
	self announcer
		when: SpWidgetBuilt send: #setRefreshedToTrue to: self.
	
	canBeRefreshed := false.
	self whenWindowChanged: [ :w |
		w whenClosedDo: [ self inspector close ] ].
]

{ #category : #initialization }
SpInspectorNavigator >> initializeShortcuts [
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
SpInspectorNavigator >> initializeWidgets [

	
	toolbar := self instantiate: SpEyeInspectorToolBar.
]

{ #category : #actions }
SpInspectorNavigator >> inspect: anObject [
	| oldInspectorType |
	oldInspectorType := self inspectorType.
	self 
		inspector: (SpEyeInspector inspector: anObject);
		inspectorType: oldInspectorType.

]

{ #category : #history }
SpInspectorNavigator >> inspectHistoryItem: historyItem [
	"self inspect: historyItem key .
	self selectedIndex: historyItem value."
	self inspector: historyItem key
]

{ #category : #actions }
SpInspectorNavigator >> inspectNext [
	history hasNext
		ifFalse: [ ^ self ].
	self inspectHistoryItem: history next
]

{ #category : #actions }
SpInspectorNavigator >> inspectPrevious [
	history hasPrevious
		ifFalse: [ ^ self ].
	self inspectHistoryItem: history previous
]

{ #category : #accessing }
SpInspectorNavigator >> inspector [
	^ inspector value
]

{ #category : #accessing }
SpInspectorNavigator >> inspector: anEyeInspector [
	self basicInspector: anEyeInspector.
	self pushHistory.
]

{ #category : #accessing }
SpInspectorNavigator >> inspectorType [
	^ self inspector class
]

{ #category : #accessing }
SpInspectorNavigator >> inspectorType: anInspectorClass [
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
SpInspectorNavigator >> inspectorTypes [
	^ self inspector variants
]

{ #category : #accessing }
SpInspectorNavigator >> nextInspectorType [
	| index types |
	types := self inspectorTypes.
	index := types 
		indexOf: self inspectorType
		ifAbsent: [ ^ types first ].
	^types atWrap: index + 1.
]

{ #category : #accessing }
SpInspectorNavigator >> object [
	^ self inspector object
]

{ #category : #accessing }
SpInspectorNavigator >> previousInspectorType [
	| index types |
	types := self inspectorTypes.
	index := types 
		indexOf: self inspectorType
		ifAbsent: [ ^ types first ].
	^types atWrap: index - 1.
]

{ #category : #history }
SpInspectorNavigator >> pushHistory [
	history add: self inspector.
	self toolbar update.
]

{ #category : #accessing }
SpInspectorNavigator >> selectedIndex [
	^ self inspector selectedIndex
]

{ #category : #accessing }
SpInspectorNavigator >> selectedIndex: anInteger [
	self inspector selectedIndex: anInteger
]

{ #category : #accessing }
SpInspectorNavigator >> selectedObject [
	^ self inspector selectedObject
]

{ #category : #private }
SpInspectorNavigator >> setRefreshedToTrue [
	"introduced because we should not use weak announcer with on:do:."
	
	canBeRefreshed := true
]

{ #category : #initialization }
SpInspectorNavigator >> step [
	self inspector step.
]

{ #category : #initialization }
SpInspectorNavigator >> stepTime [
	^ 1500
]

{ #category : #updating }
SpInspectorNavigator >> takeKeyboardFocus [
	^ self inspector takeKeyboardFocus
]

{ #category : #accessing }
SpInspectorNavigator >> taskbarIcon [
	^ self inspector 
		ifNil: [ super taskbarIcon ]
		ifNotNil: [ :anInspector | anInspector taskbarIcon ]
]

{ #category : #accessing }
SpInspectorNavigator >> title [
	^ self inspector title
]

{ #category : #accessing }
SpInspectorNavigator >> toolbar [
	^ toolbar
]

{ #category : #accessing }
SpInspectorNavigator >> toolbar: anObject [ 
	toolbar := anObject 
]

{ #category : #updating }
SpInspectorNavigator >> updateInspectorFrom: oldInspector [

	self updateTitle.
	self inspector ifNil: [ ^ self ].

	self canBeRefreshed ifFalse: [ ^ self ].

	self needRebuild: false.

	self buildWithSpec: oldInspector spec selector.
	
	oldInspector close. "kill the update process"
]

{ #category : #actions }
SpInspectorNavigator >> useBasicInspector [
	self inspectorType: SpEyeBasicInspector
]

{ #category : #actions }
SpInspectorNavigator >> useDefaultInspector [
	self inspectorType: self object class inspectorClass
]

{ #category : #actions }
SpInspectorNavigator >> useInspectorAt: index [
	"choose an inspector at the given index from the given inspectorTypes.
	If the index is out of bounds, simply crop it to the neares value"
	| types |
	types := self inspectorTypes.
	self inspectorType: (types at: (index min: types size max: 1))
]

{ #category : #actions }
SpInspectorNavigator >> useNextInspectorType [
	self inspectorType: self nextInspectorType
]

{ #category : #actions }
SpInspectorNavigator >> usePreviousInspectorType [
	self inspectorType: self previousInspectorType
]

{ #category : #actions }
SpInspectorNavigator >> useTreeInspector [
	self inspectorType: SpEyeTreeInspector
]
