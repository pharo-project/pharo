"
I am a tree inspector that takes the EyeElements from the default inspectors and displays them in a tree.
"
Class {
	#name : #EyeTreeInspector,
	#superclass : #EyeAbstractInspector,
	#instVars : [
		'tree'
	],
	#category : #'EyeInspector-Base'
}

{ #category : #spec }
EyeTreeInspector class >> inspectorSpec [

	<spec>
	^ SpPanedLayout newTopToBottom
		  positionOfSlider: 70 percent;
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

	self text beForObject: self selectedElement value
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
EyeTreeInspector >> inspectionMenu [
	| menuPresenter |

	self hasSelectedObject ifFalse: [ ^ nil ].
	menuPresenter := SpMenuPresenter new.
	self inspectionSubMenu: menuPresenter.
	self selectedElement inspectionMenu: menuPresenter.

	^ menuPresenter
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

	self text beForObject: self object.
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

{ #category : #'private - focus' }
EyeTreeInspector >> takeKeyboardFocus [
	^ self tree takeKeyboardFocus
]

{ #category : #accessing }
EyeTreeInspector >> tree [

	^ tree ifNil: [ 
		tree := self newTreeTable
			addColumn: (SpCompositeTableColumn new 
				title: 'Instance'; 
				addColumn: ((SpImageTableColumn evaluated: [ :node | self iconFor: node ])
					width: 20;
					yourself);
				addColumn: (SpStringTableColumn evaluated: [ :node | self labelFor: node ]);
				yourself);
			children: [ :node | self childrenFor: node ];
			"menu: [ :menu | self inspectionMenu: menu ];"
			whenSelectedItemChangedDo: [ :item | item ifNotNil: [ self elementChanged ] ];
			whenActivatedDo: [ self diveInto: self selectedObject ];
			whenBuiltDo: [ self initializeShortcuts ];
			yourself ]
]
