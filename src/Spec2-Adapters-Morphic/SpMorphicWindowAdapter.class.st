"
I am the adapter used to bridge a WindowPresenter and a SystemWindow
"
Class {
	#name : #SpMorphicWindowAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'widget API' }
SpMorphicWindowAdapter >> aboutText [

	^ self model model
		ifNotNil: [ :m | m aboutText ]
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> aboutTitle [
    ^ self model model
         ifNotNil: [ :m | (m respondsTo: #aboutTitle)
		ifTrue: [m aboutTitle]
		ifFalse: [m class name]].
]

{ #category : #private }
SpMorphicWindowAdapter >> add: containerMorph toWindow: aSpecWindow [

	aSpecWindow 
		addMorph: containerMorph 
		fullFrame: LayoutFrame identity
]

{ #category : #private }
SpMorphicWindowAdapter >> addContent: aMorph toWindow: aSpecWindow [
	| containerMorph |
	
	containerMorph := self newContainerMorph.
	containerMorph layoutFrame: aMorph layoutFrame.

	"add all decorations (menu, toolbar and statusbar)"
	self model hasMenu ifTrue: [ self addMenuTo: containerMorph ].
	self model hasToolbar ifTrue: [ self addToolBarTo: containerMorph ].
	containerMorph addMorphBack: aMorph.
	aMorph
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	self model hasStatusBar ifTrue: [ self addStatusBarTo: containerMorph ].
	
	self add: containerMorph toWindow: aSpecWindow
]

{ #category : #initialization }
SpMorphicWindowAdapter >> addFocusRotationKeyBindings [
	"Windows should not process keybindings (because then the active widget will not get 
	 the event"
]

{ #category : #private }
SpMorphicWindowAdapter >> addMenuTo: aMorph [
	| menuMorph |

	menuMorph := self model menu buildWithSpec.
	aMorph addMorphBack: menuMorph.	
	menuMorph
		hResizing: #spaceFill;
		vResizing: #rigid
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> addModelItemsToWindowMenu: aMenu [

	self model addMenuItemsToWindowMenu: aMenu
]

{ #category : #protocol }
SpMorphicWindowAdapter >> addPresenterIn: widgetToBuild withSpecLayout: aSpec [

	self model initialPosition 
		ifNotNil: [ :position | widgetToBuild position: position ].
	self model initialExtent 
		ifNotNil: [ :extent | widgetToBuild extent: extent ].
	self 
		addContent: (self model presenter buildWithSpecLayout: aSpec) 
		toWindow: widgetToBuild
]

{ #category : #private }
SpMorphicWindowAdapter >> addStatusBarTo: aMorph [
	| statusBarMorph |

	statusBarMorph := self model statusBar buildWithSpec.
	aMorph addMorphBack: statusBarMorph.	
	statusBarMorph
		hResizing: #spaceFill;
		vResizing: #rigid
]

{ #category : #private }
SpMorphicWindowAdapter >> addToolBarTo: aMorph [
	| toolbarMorph |

	toolbarMorph := self model toolBar buildWithSpec.
	aMorph addMorphBack: toolbarMorph.	
	toolbarMorph
		hResizing: #spaceFill;
		vResizing: #rigid
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> askOkToClose [

	^ self model askOkToClose
]

{ #category : #protocol }
SpMorphicWindowAdapter >> beep [ 
	
	widget flash
]

{ #category : #factory }
SpMorphicWindowAdapter >> buildWidget [

	^ SpWindow new
		model: model;
		isResizeable: self isResizeable;
		in: [ :this | self subscribeToAnnouncements: this ];
		yourself
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> cancelled [

	^ false
]

{ #category : #protocol }
SpMorphicWindowAdapter >> centered [

	self widgetDo: [ :w | 
		w
			fullBounds;
			position: Display extent - w extent // 2 ]
]

{ #category : #protocol }
SpMorphicWindowAdapter >> close [

	self widget ifNotNil: [ :w | w close ].
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> delete [

	self widgetDo: [ :w | 
		w delete. 
		self application windowClosed: self ]
]

{ #category : #protocol }
SpMorphicWindowAdapter >> extent: aPoint [

	^ self widget extent: aPoint
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> initialExtent [

	^ self model initialExtent
]

{ #category : #protocol }
SpMorphicWindowAdapter >> isDisplayed [

	^ self widget isDisplayed
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> isResizeable [

	^ self model isResizeable
]

{ #category : #'spec protocol' }
SpMorphicWindowAdapter >> isResizeable: aBoolean [

	^ self widgetDo: [ :w | w isResizeable: aBoolean ]
]

{ #category : #'spec protocol' }
SpMorphicWindowAdapter >> maximize [

	self widgetDo: [ :w | w maximize ]
]

{ #category : #'spec protocol' }
SpMorphicWindowAdapter >> minimize [

	self widgetDo: [ :w | w minimize ]
]

{ #category : #private }
SpMorphicWindowAdapter >> newContainerMorph [
		
	^ PanelMorph new
		layoutPolicy: TableLayout new; 
		hResizing: #spaceFill;
		vResizing: #spaceFill;		
		listDirection: #topToBottom; 
		yourself
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> okToChange [
^ self model okToChange
]

{ #category : #protocol }
SpMorphicWindowAdapter >> open [

	self model isCentered 
		ifTrue: [ self centered ].
		
	self model initialPosition 
		ifNotNil: [ self widget openAsIs ]
		ifNil: [ self widget openInWorld ].
	
	self windowIsOpening
]

{ #category : #protocol }
SpMorphicWindowAdapter >> rebuildWithSpecLayout: aSpec [
	| sub |
	
	sub := self model window submorphs copy.
	self model window removeAllMorphs.
	sub allButLast do: [:e | self model window addMorphBack: e ].
	self model addPresenterIn: self widget withSpecLayout: aSpec.
	self widget model: self
]

{ #category : #factory }
SpMorphicWindowAdapter >> subscribeToAnnouncements: aWindow [

	aWindow announcer 
		when: SpWindowWillClose send: #announce: to: self presenter announcer;
		when: WindowResizing send: #announce: to: self presenter announcer
]

{ #category : #icons }
SpMorphicWindowAdapter >> taskbarIcon [ 
	^ self model taskbarIcon
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> title [

	^ self model title
]

{ #category : #'widget API' }
SpMorphicWindowAdapter >> title: aString [

	self widgetDo: [ :w | w setLabel: aString ]
]

{ #category : #updating }
SpMorphicWindowAdapter >> windowIsClosing [

	self model windowIsClosing
]

{ #category : #updating }
SpMorphicWindowAdapter >> windowIsOpening [

	self model windowIsOpening
]
