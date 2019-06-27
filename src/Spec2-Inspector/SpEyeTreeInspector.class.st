"
I am a tree inspector that takes the EyeElements from the default inspectors and displays them in a tree.
"
Class {
	#name : #SpEyeTreeInspector,
	#superclass : #SpEyeAbstractInspector,
	#instVars : [
		'tree'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #spec }
SpEyeTreeInspector class >> inspectorSpec [
	<spec>
	
	^ SpLayout composed
		newRow: #tree bottom: 0.3;
		addHSplitter;
		newRow: #text top: 0.7;
		yourself
]

{ #category : #accessing }
SpEyeTreeInspector class >> label [
	^ 'Tree Inspector'
]

{ #category : #api }
SpEyeTreeInspector >> childrenFor: anEyeElement [
	^ self childrenForObject: anEyeElement value
]

{ #category : #api }
SpEyeTreeInspector >> childrenForObject: anObject [
	^ (SpEyeInspector inspector: anObject) elements 
			select: [ :element | element shouldShowInTree ]
]

{ #category : #'event-handling' }
SpEyeTreeInspector >> elementChanged [
	self text doItReceiver: self selectedElement value.
	self text ifNotNil: [ :w | w behavior: self selectedElement value class ]
]

{ #category : #api }
SpEyeTreeInspector >> expandRoots [
	self tree expandRoots
]

{ #category : #api }
SpEyeTreeInspector >> iconFor: anEyeElement [
	anEyeElement withErrorsDo: [ :error |
		^ self iconFor: anEyeElement error: error ].
		
	^ [ anEyeElement icon ]
		on: Error do: [ :error |
			self iconFor: anEyeElement error: error ]
]

{ #category : #api }
SpEyeTreeInspector >> iconFor: anEyeElement error: error [
	^ self iconNamed: #smallWarningIcon
]

{ #category : #initialization }
SpEyeTreeInspector >> initializeShortcuts [
	"initialize the inspector' shortcuts with the dictionary defined in self shortCuts"
	
	self shortCuts keysAndValuesDo: [ :key :value |
		self tree bindKeyCombination: key toAction: value ]
]

{ #category : #menu }
SpEyeTreeInspector >> inspectionMenu: aMenu [
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	self inspectionSubMenu: aMenu.
	self selectedElement inspectionMenu: aMenu.
	^aMenu.
]

{ #category : #api }
SpEyeTreeInspector >> labelFor: anEyeElement [
	"Take care of errors that might occur during label access"
	^ [ anEyeElement longLabel ]
		on: Error do: [ :error |
			anEyeElement label, ': ', (self labelFor: anEyeElement error: error) ]
]

{ #category : #'event-handling' }
SpEyeTreeInspector >> objectChanged [
	self text doItReceiver: self object.
	self text ifNotNil: [ :w | w behavior: self object class ].
	self tree
		roots: self roots;
		expandRoots
]

{ #category : #'event-handling' }
SpEyeTreeInspector >> ownerChanged [
	super ownerChanged.
	self expandRoots.

]

{ #category : #accessing }
SpEyeTreeInspector >> roots [
	^ { SpRootEyeElement host: self object }
]

{ #category : #accessing }
SpEyeTreeInspector >> selectedElement [
	self tree hasSelectedItem
		ifFalse: [ ^ nil ].
	^ self tree selectedItem content
]

{ #category : #'smartSuggestions - support' }
SpEyeTreeInspector >> selectedMessage [
	^ nil
]

{ #category : #accessing }
SpEyeTreeInspector >> selectedObject [
	^self selectedElement value
]

{ #category : #'private-focus' }
SpEyeTreeInspector >> takeKeyboardFocus [
	^ self tree takeKeyboardFocus
]

{ #category : #accessing }
SpEyeTreeInspector >> tree [
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
