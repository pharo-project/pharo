"
I am a tree inspector that takes the EyeElements from the default inspectors and displays them in a tree.
"
Class {
	#name : #EyeTreeInspector,
	#superclass : #EyeAbstractInspector,
	#instVars : [
		'tree'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #spec }
EyeTreeInspector class >> inspectorSpec [
	<spec>
	^ SpPanedLayout newVertical
		position: 70 percent;
		add: #tree;
		add: #text;
		yourself
]

{ #category : #accessing }
EyeTreeInspector class >> label [
	^ 'Tree Inspector'
]

{ #category : #api }
EyeTreeInspector >> childrenFor: anEyeElement [
	^ self childrenForObject: anEyeElement value
]

{ #category : #api }
EyeTreeInspector >> childrenForObject: anObject [
	^ (EyeInspector inspector: anObject) elements 
			select: [ :element | element shouldShowInTree ]
]

{ #category : #'event-handling' }
EyeTreeInspector >> elementChanged [
	self text doItReceiver: self selectedElement value.
	self text ifNotNil: [ :w | w behavior: self selectedElement value class ]
]

{ #category : #api }
EyeTreeInspector >> expandRoots [
	self tree expandRoots
]

{ #category : #api }
EyeTreeInspector >> iconFor: anEyeElement [
	anEyeElement withErrorsDo: [ :error |
		^ self iconFor: anEyeElement error: error ].
		
	^ [ anEyeElement icon ]
		on: Error do: [ :error |
			self iconFor: anEyeElement error: error ]
]

{ #category : #api }
EyeTreeInspector >> iconFor: anEyeElement error: error [
	^ self iconNamed: #smallWarningIcon
]

{ #category : #initialization }
EyeTreeInspector >> initializeShortcuts [
	"initialize the inspector' shortcuts with the dictionary defined in self shortCuts"
	
	self shortCuts keysAndValuesDo: [ :key :value |
		self tree bindKeyCombination: key toAction: value ]
]

{ #category : #menu }
EyeTreeInspector >> inspectionMenu: aMenu [
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	self inspectionSubMenu: aMenu.
	self selectedElement inspectionMenu: aMenu.
	^aMenu.
]

{ #category : #api }
EyeTreeInspector >> labelFor: anEyeElement [
	"Take care of errors that might occur during label access"
	^ [ anEyeElement longLabel ]
		on: Error do: [ :error |
			anEyeElement label, ': ', (self labelFor: anEyeElement error: error) ]
]

{ #category : #'event-handling' }
EyeTreeInspector >> objectChanged [
	self text doItReceiver: self object.
	self text ifNotNil: [ :w | w behavior: self object class ].
	self tree
		roots: self roots;
		expandRoots
]

{ #category : #'event-handling' }
EyeTreeInspector >> ownerChanged [
	super ownerChanged.
	self expandRoots.

]

{ #category : #accessing }
EyeTreeInspector >> roots [
	^ { RootEyeElement host: self object }
]

{ #category : #accessing }
EyeTreeInspector >> selectedElement [
	self tree hasSelectedItem
		ifFalse: [ ^ nil ].
	^ self tree selectedItem content
]

{ #category : #'smartSuggestions - support' }
EyeTreeInspector >> selectedMessage [
	^ nil
]

{ #category : #accessing }
EyeTreeInspector >> selectedObject [
	^self selectedElement value
]

{ #category : #'private-focus' }
EyeTreeInspector >> takeKeyboardFocus [
	^ self tree takeKeyboardFocus
]

{ #category : #accessing }
EyeTreeInspector >> tree [
	^ tree
		ifNil: [ tree := self instantiate: SpTreePresenter.
			tree whenBuiltDo: [ self initializeShortcuts ].
			tree
				whenSelectedItemChanged: [ :item | item ifNotNil: [ self elementChanged ] ];
				childrenBlock: [ :anObject | self childrenFor: anObject ];
				iconBlock: [ :treeNode | self iconFor: treeNode ];
				displayBlock: [ :anObject | self labelFor: anObject ];
				menu: [ :menu | self inspectionMenu: menu ];
				doubleClick: [ self diveInto: self selectedObject ];
				yourself ]
]
