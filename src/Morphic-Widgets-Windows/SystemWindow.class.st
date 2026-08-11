"
SystemWindow is the Morphic equivalent of StandardSystemView -- a labelled container for rectangular views, with iconic facilities for close, collapse/expand, and resizing.

The attribute onlyActiveOnTop, if set to true (and any call to activate will set this), determines that only the top member of a collection of such windows on the screen shall be active.  To be not active means that a mouse click in any region will only result in bringing the window to the top and then making it active.
"
Class {
	#name : 'SystemWindow',
	#superclass : 'ModelMorph',
	#instVars : [
		'labelString',
		'stripes',
		'label',
		'closeBox',
		'collapseBox',
		'paneMorphs',
		'collapsedFrame',
		'fullFrame',
		'isCollapsed',
		'menuBox',
		'mustNotClose',
		'labelWidgetAllowance',
		'updatablePanes',
		'labelArea',
		'expandBox',
		'embeddable',
		'isResizeable',
		'taskbarTask'
	],
	#classVars : [
		'CloseBoxImage',
		'CollapseBoxImage',
		'FullscreenMargin',
		'TopWindow',
		'UseHideForClose'
	],
	#category : 'Morphic-Widgets-Windows-Widgets',
	#package : 'Morphic-Widgets-Windows',
	#tag : 'Widgets'
}

{ #category : 'accessing' }
SystemWindow class >> borderWidth [

	"Making changes to this for some reason requires repositioning of CornerGripMorphs.
	Edit BorderedMorph#addCornerGrip and play with offsets to get them right if you increase
	border width. For instance, going from 4 to 6 here and you should updated offsets to
	(-23@-23 corner: 0@0) for the right placement of corner grips."

	^ self theme windowBorderWidthFor: self
]

{ #category : 'keymapping' }
SystemWindow class >> buildShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #close)
		category: #WindowShortcuts
		default: PharoShortcuts current closeWindowShortcut
		do: [ :target | target delete ]
		description: 'Close this window'
]

{ #category : 'keymapping' }
SystemWindow class >> buildWindowBottomEdgeShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #windowBottomEdgeUp)
		category: #WindowShortcuts
		default: PharoShortcuts current windowBottomEdgeUpShortcut
		do: [ :target |
			| movement |
			movement := 0@(target bounds height / 2) negated.
			target bounds: ((target bounds topLeft) corner: (target bounds bottomRight + movement))]
		description: 'Move bottom edge of the window up by half of the window extent'.

	(aBuilder shortcut: #windowBottomEdgeDown)
		category: #WindowShortcuts
		default: PharoShortcuts current windowBottomEdgeDownShortcut
		do: [ :target |
			| movement |
			movement := 0@(target bounds height / 2).
			target bounds: ((target bounds topLeft) corner: (target bounds bottomRight + movement))]
		description: 'Move bottXX edge of the window up by half of the window extent'
]

{ #category : 'keymapping' }
SystemWindow class >> buildWindowLeftEdgeShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #windowLeftEdgeLeft)
		category: #WindowShortcuts
		default: PharoShortcuts current windowLeftEdgeLeftShortcut
		do: [ :target |
			| movement |
			movement := (target bounds width / 2) negated @0.
			target bounds: ((target bounds topLeft + movement) corner: (target bounds bottomRight ))]
		description: 'Move left edge of the window left by half of the window extent'.

	(aBuilder shortcut: #windowLeftEdgeRight)
		category: #WindowShortcuts
		default: PharoShortcuts current windowLeftEdgeRightShortcut
		do: [ :target |
			| movement |
			movement := (target bounds width / 2)@0.
			target bounds: ((target bounds topLeft + movement) corner: (target bounds bottomRight))]
		description: 'Move left edge of the window right by half of the window extent'
]

{ #category : 'keymapping' }
SystemWindow class >> buildWindowMovingShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #windowMoveLeft)
		category: #WindowShortcuts
		default: PharoShortcuts current windowMoveLeftShortcut
		do: [ :target |
			| movement |
			movement := ((target bounds width / 2) negated)@0.
			target bounds: ((target bounds topLeft + (movement)) corner: (target bounds bottomRight + (movement)))]
		description: 'Move window left by half of its extent'.

	(aBuilder shortcut: #windowMoveRight)
		category: #WindowShortcuts
		default: PharoShortcuts current windowMoveRightShortcut
		do: [ :target |
			| movement |
			movement := ((target bounds width /2))@0.
			target bounds: ((target bounds topLeft + (movement)) corner: (target bounds bottomRight + (movement)))]
		description: 'Move window right by half of its extent'.

	(aBuilder shortcut: #windowMoveUp)
		category: #WindowShortcuts
		default: PharoShortcuts current windowMoveUpShortcut
		do: [ :target |
			| movement |
			movement := 0@((target bounds height / 2) negated).
			target bounds: ((target bounds topLeft + (movement)) corner: (target bounds bottomRight + (movement)))]
		description: 'Move window up by half of its extent'.

	(aBuilder shortcut: #windowMoveDown)
		category: #WindowShortcuts
		default: PharoShortcuts current windowMoveDownShortcut
		do: [ :target |
			| movement |
			movement := 0@(target bounds height / 2).
			target bounds: ((target bounds topLeft + (movement)) corner: (target bounds bottomRight + (movement)))]
		description: 'Move window down by half of its extent'
]

{ #category : 'keymapping' }
SystemWindow class >> buildWindowRightEdgeShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #windowRightEdgeLeft)
		category: #WindowShortcuts
		default: PharoShortcuts current windowRightEdgeLeftShortcut
		do: [ :target |
			| movement |
			movement := (target bounds width / 2) negated @0.
			target bounds: ((target bounds topLeft) corner: (target bounds bottomRight + movement))]
		description: 'Center the window with a default extent'.

	(aBuilder shortcut: #windowRightEdgeRight)
		category: #WindowShortcuts
		default: PharoShortcuts current windowRightEdgeRightShortcut
		do: [ :target |
			| movement |
			movement := (target bounds width / 2)@0.
			target bounds: ((target bounds topLeft) corner: (target bounds bottomRight + movement))]
		description: 'Center the window with a default extent'
]

{ #category : 'keymapping' }
SystemWindow class >> buildWindowTilingShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #windowTopLeft)
		category: #WindowShortcuts
		default: PharoShortcuts current windowTopLeftShortcut
		do: [ :target | target tileTopLeft]
		description: 'Move the window to the top left corner of the display'.

	(aBuilder shortcut: #windowLeft)
		category: #WindowShortcuts
		default: PharoShortcuts current windowLeftShortcut
		do: [ :target | target tileLeft ]
		description: 'Move the window to the left half of the display'.

	(aBuilder shortcut: #windowBottomLeft)
		category: #WindowShortcuts
		default: PharoShortcuts current windowBottomLeftShortcut
		do: [ :target | target tileBottomLeft ]
		description: 'Move the window to the bottom left corner of the display'.

	(aBuilder shortcut: #windowTop)
		category: #WindowShortcuts
		default: PharoShortcuts current windowTopShortcut
		do: [ :target | target tileTop ]
		description: 'Move the window to the top half of the display'.

	(aBuilder shortcut: #windowTopRight)
		category: #WindowShortcuts
		default: PharoShortcuts current windowTopRightShortcut
		do: [ :target | target tileTopRight ]
		description: 'Move the window to the top right corner of the display'.

	(aBuilder shortcut: #windowRight)
		category: #WindowShortcuts
		default: PharoShortcuts current windowRightShortcut
		do: [ :target | target tileRight ]
		description: 'Move the window to the right half of the display'.

	(aBuilder shortcut: #windowRightBottom)
		category: #WindowShortcuts
		default: PharoShortcuts current windowRightBottomShortcut
		do: [ :target | target tileBottomRight ]
		description: 'Move the window to the right bottom corner of the display'.

	(aBuilder shortcut: #windowBottom)
		category: #WindowShortcuts
		default: PharoShortcuts current windowBottomShortcut
		do: [ :target | target tileBottom ]
		description: 'Move the window to the bottom half of the display'.

	(aBuilder shortcut: #windowMaximize)
		category: #WindowShortcuts
		default: PharoShortcuts current windowMaximizeShortcut
		do: [ :target | target tileFull ]
		description: 'Maximize the window'.

	(aBuilder shortcut: #windowMinimize)
		category: #WindowShortcuts
		default: PharoShortcuts current windowMinimizeShortcut
		do: [ :target | target minimize]
		description: 'Miminimze the window'.

	(aBuilder shortcut: #windowCenter)
		category: #WindowShortcuts
		default: PharoShortcuts current windowCenterShortcut
		do: [ :target | target tileCenter]
		description: 'Center the window with a default extent'
]

{ #category : 'keymapping' }
SystemWindow class >> buildWindowTopEdgeShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #windowTopEdgeUp)
		category: #WindowShortcuts
		default: PharoShortcuts current windowTopEdgeUpShortcut
		do: [ :target |
			| movement |
			movement := 0@(target bounds height / 2) negated.
			target bounds: ((target bounds topLeft + movement) corner: (target bounds bottomRight))]
		description: 'Move top edge of the window up by half of the window extent'.

	(aBuilder shortcut: #windowTopEdgeDown)
		category: #WindowShortcuts
		default: PharoShortcuts current windowTopEdgeDownShortcut
		do: [ :target |
			| movement |
			movement := 0@(target bounds height / 2).
			target bounds: ((target bounds topLeft + movement) corner: (target bounds bottomRight))]
		description: 'Move top edge of the window down by half of the window extent'
]

{ #category : 'accessing' }
SystemWindow class >> closeBoxImage [
	"Supplied here because we don't necessarily have ComicBold"

	^ CloseBoxImage ifNil: [CloseBoxImage := (Form
	extent: 10@10
	depth: 32
	fromArray: #( 0 0 0 0 0 0 0 0 0 0 0 3326099520 3330310272 0 0 0 0 0 0 0 0 4144038145 3326099520 3330310272 0 0 0 4144038145 3326099520 0 0 0 4227924225 3326099520 3330310272 0 4144038145 3326099520 3330310272 0 0 0 0 4144038145 3326099520 4144038145 3326099520 3330310272 3336494814 0 0 0 0 0 4227924225 3326099520 3330310272 3336494814 0 0 0 0 0 4144038145 3326099520 4144038145 3326099520 3330310272 3336494814 0 0 0 4144038145 3326099520 3330310272 3336494814 4144038145 3326099520 3330310272 0 0 4144038145 3326099520 3330310272 3336494814 0 0 4144038145 3326099520 0 0 0 3330310272 3336494814 0 0 0 0 0 0)
	offset: 0@0)]
]

{ #category : 'top window' }
SystemWindow class >> closeTopWindow [

	| announcement |
	"Try to close the top window.  It may of course decline"
	TopWindow ifNotNil: [ :window |
			TopWindow := nil.
			self useHideForClose
				ifTrue: [
						window hide.
						announcement := WindowClosed new
							                window: window;
							                yourself.
						self currentWorld announcer announce: announcement ]
				ifFalse: [ window delete ] ]
]

{ #category : 'accessing' }
SystemWindow class >> collapseBoxImage [
	"Supplied here because we don't necessarily have ComicBold"

	^ CollapseBoxImage ifNil: [ CollapseBoxImage := (Form
	extent: 10@10
	depth: 32
	fromArray: #( 0 0 4127260929 4127260929 4127260929 4127260929 4127260929 0 0 0 0 3875602689 3212869760 3212869760 3212869760 3212869760 3212869760 4227924225 0 0 4127260929 3212869760 3212869760 0 0 0 0 3212869760 4127260929 0 4127260929 3212869760 0 0 0 0 0 0 4127260929 3212869760 4127260929 3212869760 0 0 0 0 0 0 4127260929 3212869760 4127260929 3212869760 0 0 0 0 0 0 4127260929 3212869760 4128708375 3212869760 0 0 0 0 0 0 4127260929 3212869760 0 4127260929 3212869760 0 0 0 0 4127260929 3208659008 3212869760 0 3208659008 4127260929 4127260929 4127260929 4127260929 4127260929 3208659008 3212869760 0 0 0 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760 0 0)
	offset: 0@0)]
]

{ #category : 'accessing' }
SystemWindow class >> expandBoxImage [

	^ (Form
	extent: 10@10
	depth: 32
	fromArray: #( 3875602689 3875602689 3875602689 3875602689 3875602689 3875602689 0 0 0 0 3875602689 0 0 0 0 4127260929 3877181721 3877181721 3875602689 0 3875602689 0 0 0 0 3875602689 3212869760 0 3875602689 3212869760 3875602689 0 0 0 0 3875602689 3212869760 0 3875602689 3212869760 3875602689 0 0 0 0 3875602689 3212869760 0 3875602689 3212869760 3875602689 4127260929 3875602689 3875602689 3875602689 3875602689 3212869760 0 3875602689 3212869760 0 3877181721 3212869760 3212869760 3212869760 3212869760 3212869760 0 3875602689 3212869760 0 3877181721 0 0 0 0 0 0 3875602689 3212869760 0 3875602689 3875602689 3875602689 3875602689 3875602689 3875602689 3875602689 3875602689 3212869760 0 0 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760)
	offset: 0@0)
]

{ #category : 'settings' }
SystemWindow class >> fullscreenMargin [
	^ FullscreenMargin ifNil: [FullscreenMargin := 0]
]

{ #category : 'settings' }
SystemWindow class >> fullscreenMargin: anInteger [
	FullscreenMargin := anInteger
]

{ #category : 'private-menu building' }
SystemWindow class >> icons [

	^ Smalltalk ui icons
]

{ #category : 'class initialization' }
SystemWindow class >> initialize [

	CollapseBoxImage := nil.
	CloseBoxImage := nil
]

{ #category : 'instance creation' }
SystemWindow class >> labelled: labelString [
	^self new setLabel: labelString
]

{ #category : 'accessing' }
SystemWindow class >> menuBoxImage [

	^ (Form
	extent: 10@10
	depth: 32
	fromArray: #( 4227858432 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4227858432 0 4127195136 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760 4127195136 3212869760 4127195136 3212869760 0 0 0 0 0 0 4127195136 3212869760 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 3212869760 4127195136 3212869760 0 0 0 0 0 0 4127195136 3212869760 4127195136 3212869760 0 0 0 0 0 0 4127195136 3212869760 4227858432 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 3212869760 4127195136 3212869760 0 0 0 0 0 0 4127195136 3212869760 4227858432 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4127195136 4227858432 3212869760 0 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760 3212869760)
	offset: 0@0)
]

{ #category : 'top window' }
SystemWindow class >> noteTopWindowIn: aWorld [
	"Clear the current top window and activate the topmost visible window"

	TopWindow := nil.

	aWorld ifNil: [^ self].
	aWorld submorphsDo: [:m |
		(m isSystemWindow and: [m isCollapsed not]) ifTrue: [
			^m activate]]
]

{ #category : 'activation' }
SystemWindow class >> passivateTopWindow [
	"Make no window the top window."

	TopWindow ifNotNil: [ :window |
		TopWindow := nil.
		window privateBePassive.
	]
]

{ #category : 'initialization' }
SystemWindow class >> resetForms [
	"Set the box forms to nil so that they will be taken from the current theme.
	Don't touch the expand or menu ones, delegated to theme anyway."

	CloseBoxImage := nil.
	CollapseBoxImage := nil
]

{ #category : 'top window' }
SystemWindow class >> sendTopWindowToBack [
 	"Send the top window of the world to the back. Relies on #submorphs array being ordered top to bottom. "
	self currentWorld submorphs
		detect: [ :morph | morph isSystemWindow ]
		ifFound: [ :morph | morph sendToBack ]
]

{ #category : 'menu items - tiling' }
SystemWindow class >> tilingMenuItemsOn: aBuilder [

	<windowMenu>

	(aBuilder item: #tileLeft)
		action: [ :target | target tileLeft ];
		label: 'Left';
		parent: #Tile;
		icon: (self iconNamed: #blank);
		order: 1;
		help: 'Move the window to the left half of the display'.

	(aBuilder item: #tileRight)
		action: [ :target | target tileRight ];
		label: 'Right';
		parent: #Tile;
		help: 'Move the window to the right half of the display'.

	(aBuilder item: #tileTop)
		action: [ :target | target tileTop ];
		label: 'Top';
		parent: #Tile;
		help: 'Move the window to the top half of the display'.

	(aBuilder item: #tileBottom)
		action: [ :target | target tileBottom ];
		label: 'Bottom';
		parent: #Tile;
		help: 'Move the window to the bottom half of the display'.

	(aBuilder item: #tileTopLeft)
		action: [ :target | target tileTopLeft ];
		label: 'Top left';
		parent: #Tile;
		help: 'Move the window to the top left corner of the display'.

	(aBuilder item: #tileTopRight)
		action: [ :target | target tileTopRight ];
		label: 'Top right';
		parent: #Tile;
		help: 'Move the window to the top right corner of the display'.

	(aBuilder item: #tileBottomLeft)
		action: [ :target | target tileBottomLeft ];
		label: 'Bottom left';
		parent: #Tile;
		help: 'Move the window to the bottom left corner of the display'.

	(aBuilder item: #tileBottomRight)
		action: [ :target | target tileBottomRight ];
		label: 'Bottom right';
		parent: #Tile;
		help: 'Move the window to the right bottom corner of the display'.

	(aBuilder item: #tileFull)
		action: [ :target | target tileFull ];
		label: 'Full';
		parent: #Tile;
		help: 'Maximize the window'.

	(aBuilder item: #tileCenter)
		action: [ :target | target tileCenter ];
		label: 'Center';
		parent: #Tile;
		help: 'Center the window with a default extent'
]

{ #category : 'menu items - tiling' }
SystemWindow class >> tilingMenuOn: aBuilder [

	<windowMenu>

	(aBuilder item: #Tile)
		order: 10.5
]

{ #category : 'top window' }
SystemWindow class >> topWindow [
	^ TopWindow
]

{ #category : 'accessing' }
SystemWindow class >> useHideForClose [

	^ UseHideForClose ifNil: [ UseHideForClose := false ]
]

{ #category : 'setting' }
SystemWindow class >> useHideForClose: aBoolean [

	UseHideForClose := aBoolean 
]

{ #category : 'menu items' }
SystemWindow class >> windowMenuOn: aBuilder [
	"To inject your custom items in a specific place, note that each item increases in order by 1. For example, if you wanted your item to be the second item, you could give it an #order: 1.5"
 
	<windowMenu>
	| closableLabel draggableLabel maximizeLabel |
	
	(aBuilder item: #Destroy)
		order: 1.0;
		action: [ aBuilder model closeBoxHit ];
		iconName: #windowClose;
		enabledBlock: [ aBuilder model allowedToClose ].
	(aBuilder item: #About)
		action: [ aBuilder model showAbout ];
		iconName: #smallHelp;
		withSeparatorAfter.
	(aBuilder item: #'Change title') action: [ aBuilder model relabel ].
	(aBuilder item: #'Copy title')
		action: [ aBuilder model copyTitle ];
		withSeparatorAfter.
	(aBuilder item: #'Send to back') action: [ aBuilder model sendToBack ].
	(aBuilder item: #'Make next-to-topmost')
		action: [ aBuilder model makeSecondTopmost ];
		withSeparatorAfter.
	(aBuilder item: #'Create window group') action: [
		aBuilder model createWindowGroup ].
	(aBuilder item: #'Open as external window')
		action: [ aBuilder model openAsExternalWindow ];
		withSeparatorAfter.
	closableLabel := aBuilder model mustNotClose
		                 ifFalse: [ #'Make unclosable' ]
		                 ifTrue: [ #'Make closable' ].
	(aBuilder item: closableLabel) action: [
		closableLabel = #'Make unclosable'
			ifTrue: [ aBuilder model makeUnclosable ]
			ifFalse: [ aBuilder model makeClosable ] ].
	draggableLabel := aBuilder model isSticky
		                  ifTrue: [ #'Make draggable' ]
		                  ifFalse: [ #'Make undraggable' ].
	(aBuilder item: draggableLabel)
		action: [ aBuilder model toggleStickiness ];
		withSeparatorAfter.
	maximizeLabel := aBuilder model isMaximized
		                 ifTrue: [ #Restore ]
		                 ifFalse: [ #Maximize ].
	(aBuilder item: maximizeLabel)
		action: [ aBuilder model expandBoxHit ];
		iconName: #windowMaximize
]

{ #category : 'top window' }
SystemWindow class >> windowsIn: aWorld satisfying: windowBlock [
	| windows |

	windows := OrderedCollection new.
	aWorld ifNil: [^windows].	"opening MVC in Morphic - WOW!"
	aWorld submorphs do:
		[:m | | s |
		((m isSystemWindow) and: [windowBlock value: m])
			ifTrue: [windows addLast: m]
			ifFalse: [((m isKindOf: TransformationMorph) and: [m submorphs size = 1])
					ifTrue: [s := m firstSubmorph.
							((s isSystemWindow) and: [windowBlock value: s])
								ifTrue: [windows addLast: s]]]].
	^ windows
]

{ #category : 'accessing' }
SystemWindow >> aboutText [
	"Answer the text to use for the About dialog."

	^self model
		ifNil: ['This is a system window without a model' translated]
		ifNotNil: [self model class instanceSide comment
					ifEmpty: ['The model of this window has no class comment']
					ifNotEmpty: [:comment | comment]]
]

{ #category : 'accessing' }
SystemWindow >> aboutTitle [
	"Answer the title to use for the About dialog."

	|title|
	title := self model
		ifNil: ['SystemWindow']
		ifNotNil: [(self model respondsTo: #aboutTitle)
					ifTrue: [self model aboutTitle]
					ifFalse: [self model class name]].
	^'About {1}' translated format: {title}
]

{ #category : 'activation' }
SystemWindow >> activate [
	"Activate the owner too."

	self activatedModalChild ifTrue: [ ^self ].

	self addPaneSplittersIfNeeded.

	super activate.

	self isEmbedded ifFalse: [
		self basicActivate
	].

	self navigateFocus.

	self positionModalOwner
]

{ #category : 'top window' }
SystemWindow >> activateAndForceLabelToShow [
	self activate.
	bounds top < 0 ifTrue:
		[self position: (self position x @ 0)]
]

{ #category : 'activation' }
SystemWindow >> activatedModalChild [

	self modalChild ifNotNil: [ :modalChild |
		modalChild owner ifNotNil: [
				modalChild activate.
				modalChild modalChild ifNil: [ modalChild indicateModalChild ].
				^true.
		]
	].
	^false
]

{ #category : 'accessing' }
SystemWindow >> activeFillStyle [
	"Return the active fillStyle for the receiver."

	^ self theme windowActiveFillStyle
]

{ #category : 'accessing' }
SystemWindow >> activeLabelFillStyle [
	"Return the active label fillStyle for the receiver."

	^self theme windowActiveLabelFillStyleFor: self
]

{ #category : 'accessing' }
SystemWindow >> activeTitleFillStyle [
	"Return the active title fillStyle for the receiver."

	^self theme windowActiveTitleFillStyleFor: self
]

{ #category : 'initialization' }
SystemWindow >> addCloseBox [
	"If I have a labelArea, add a close box to it"
	| frame |
	labelArea
		ifNil: [^ self].
	closeBox := self createCloseBox.
	frame := LayoutFrame identity leftOffset: 2.
	closeBox layoutFrame: frame.
	labelArea addMorphBack: closeBox
]

{ #category : 'controls' }
SystemWindow >> addCollapseBox [
	"If I have a labelArea, add a collapse box to it."

	labelArea
		ifNil: [^ self].
	collapseBox := self createCollapseBox.
	collapseBox layoutFrame: (self theme windowCollapseBoxLayoutFor: self).
	labelArea addMorphBack: collapseBox
]

{ #category : 'controls' }
SystemWindow >> addCornerGrips [
	"Should add these to the front!"

	|tl tr lh|
	lh := self labelHeight.
	tl  := TopLeftGripMorph new target: self; position: self position.
	tl layoutFrame topOffset: lh negated.
	tr  := TopRightGripMorph new target: self; position: self position.
	tr layoutFrame topOffset: lh negated.
	self
		addMorph: tl;
		addMorph: tr;
		addMorph: (BottomLeftGripMorph new target: self;position: self position);
		addMorph: (BottomRightGripMorph new target: self;position: self position)
]

{ #category : 'menu actions' }
SystemWindow >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
"template..."
	aCustomMenu addLine.
	aCustomMenu add: 'Edit label...' selector: #relabel
]

{ #category : 'controls' }
SystemWindow >> addEdgeGrips [
	"Should add these to the front!"

	|t l r lh|
	lh := self labelHeight.
	t  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #top.
	t layoutFrame
		topOffset: lh negated;
		bottomOffset: lh negated + self class borderWidth.
	l  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #left.
	l layoutFrame topOffset: lh negated + 22.
	r  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #right.
	r layoutFrame topOffset: lh negated + 22.
	self
		addMorph: t;
		addMorph: l;
		addMorph: r;
		addMorph: (WindowEdgeGripMorph new target: self;position: self position; edgeName: #bottom)
]

{ #category : 'controls' }
SystemWindow >> addExpandBox [
	"If I have a labelArea, add a close box to it"
	labelArea
		ifNil: [^ self].
	expandBox := self createExpandBox.
	self setExpandBoxBalloonText.
	expandBox layoutFrame: (LayoutFrame new leftFraction: 1; leftOffset: (self boxExtent x * 2 + 3) negated).
	labelArea addMorphBack: expandBox
]

{ #category : 'controls' }
SystemWindow >> addGrips [
	"Add the edge and corner grips."

	self
		addCornerGrips;
		addEdgeGrips
]

{ #category : 'controls' }
SystemWindow >> addGripsIfWanted [
	"Add the edge and corner grips if the window wants them."

	self wantsGrips ifTrue: [self addGrips]
]

{ #category : 'initialization' }
SystemWindow >> addLabelArea [

	labelArea :=self theme newWindowHeaderFor: self.
	self addMorph: labelArea
]

{ #category : 'controls' }
SystemWindow >> addMenuControl [
	"If I have a label area, add a menu control to it."

	labelArea ifNil: [^ self]. "No menu if no label area"
	menuBox
		ifNotNil: [menuBox delete].
	labelArea addMorphBack: (menuBox := self createMenuBox)
]

{ #category : 'panes' }
SystemWindow >> addMorph: aMorph frame: rectangle [

	^ self addMorph: aMorph fullFrame: rectangle
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> addMorph: aMorph fullFrame: aLayoutFrame [
	"Add a morph to the receiver with the given layout frame."

	| left right bottom top windowBorderWidth frame |
	frame := aLayoutFrame asLayoutFrame.
	windowBorderWidth := self class borderWidth.

	left := frame leftOffset.
	right := frame rightOffset.

	bottom := frame bottomOffset.
	top := frame topOffset.

	frame rightFraction = 1 ifTrue: [frame rightOffset: right - windowBorderWidth - self theme windowRightOffset].
	frame leftFraction = 0
		ifTrue: [frame leftOffset: left + windowBorderWidth + self theme windowLeftOffset]
		ifFalse: [frame leftFraction = 1 ifFalse: [
					frame leftOffset: left + ProportionalSplitterMorph splitterWidth]].

	frame bottomFraction = 1 ifTrue: [frame bottomOffset: bottom - windowBorderWidth - self theme windowBottomOffset].
	frame topFraction = 0
		ifTrue: [frame topOffset: top+ self theme windowTopOffset]
		ifFalse: [frame topFraction = 1 ifFalse: [
					frame topOffset: top + ProportionalSplitterMorph splitterWidth]].

	super addMorph: aMorph fullFrame: frame.

	paneMorphs := paneMorphs copyReplaceFrom: 1 to: 0 with: {aMorph}.
	"aMorph adoptPaneColor: self paneColor."
	aMorph
		borderStyle: (self theme windowPaneBorderStyleFor: aMorph in: self).
	self addMorphBack: aMorph. "reorder panes so flop-out right-side scrollbar is visible"

	self owner ifNotNil: [self addPaneSplitters] "do when opened for performance"
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> addPaneHSplitterBetween: topMorphs and: bottomMorphs [
	"Add a horizontal splitter for the given morphs that share a common bottom fraction.
	If there is a horizontal discontinuity apply the splitter to the first contiguous group.
	Answer the morphs to which the splitter was applied."

	|targetY fixed rightFraction leftFrame rightFrame sorted morph topGroup bottomGroup splitter offset|
	topMorphs ifEmpty: [^self].
	targetY := topMorphs first layoutFrame bottomFraction.
	fixed := topMorphs select: [:m | m layoutFrame topFraction = m layoutFrame bottomFraction].
		"fixed morphs appear in both top and bottom"
	sorted := ((topMorphs reject: [:m | m layoutFrame topFraction = m layoutFrame bottomFraction])
		asSortedCollection: [:a :b | a layoutFrame rightFraction = b layoutFrame rightFraction
			ifTrue: [a layoutFrame leftFraction <= b layoutFrame leftFraction]
			ifFalse: [a layoutFrame rightFraction <= b layoutFrame rightFraction]]) readStream.
	sorted contents ifEmpty: [^fixed].
	topGroup := OrderedCollection new.
	rightFraction := sorted contents first layoutFrame leftFraction.
	[sorted atEnd or: [morph := sorted next.
			(morph layoutFrame leftFraction ~= rightFraction and: [
				morph layoutFrame rightFraction ~= rightFraction])]] whileFalse: [
		topGroup add: morph.
		rightFraction := morph layoutFrame rightFraction].
	leftFrame := topGroup first layoutFrame.
	rightFrame := topGroup last layoutFrame.
	bottomGroup := (bottomMorphs
			reject: [:m | m layoutFrame topFraction = m layoutFrame bottomFraction])
			select: [:m |
		(m layoutFrame leftFraction
			between: leftFrame leftFraction
			and: rightFrame rightFraction) or: [
		m layoutFrame rightFraction
			between: leftFrame leftFraction
			and: rightFrame rightFraction]].
	offset := (topGroup collect: [:m | m layoutFrame bottomOffset ]) max.
	splitter := ProportionalSplitterMorph new beSplitsTopAndBottom.
	splitter layoutFrame: ((leftFrame leftFraction @ targetY corner: rightFrame rightFraction @ targetY) asLayoutFrame
									leftOffset: leftFrame leftOffset ;
									topOffset: offset;
									rightOffset: rightFrame rightOffset;
									bottomOffset: 4 + offset).
	topGroup := topGroup, fixed.
	topGroup do: [:m | splitter addLeftOrTop: m].
	bottomGroup do: [:m | splitter addRightOrBottom: m].
	self addMorphBack: splitter.
	^topGroup
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> addPaneHSplitters [
	"Add the horizontal pane splitters."

	|remaining targetBottom sameBottom sameTop|
	remaining := paneMorphs reject: [:each |
			each layoutFrame bottomFraction = 1 or: [
				each layoutFrame bottomFraction = 0]].
	[remaining notEmpty] whileTrue: [
		targetBottom := remaining first layoutFrame bottomFraction.
		sameBottom := remaining select: [:each |
			each layoutFrame bottomFraction = targetBottom].
		sameTop := paneMorphs select: [:each |
			each layoutFrame topFraction = targetBottom].
		remaining := remaining
			copyWithoutAll: (self addPaneHSplitterBetween: sameBottom and: sameTop)]
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> addPaneSplitters [
	"Add the vertical and horizontal pane splitters."

	self removePaneSplitters.
	self addPaneVSplitters.
	self addPaneHSplitters.
	self linkSplittersToSplitters
]

{ #category : 'activation' }
SystemWindow >> addPaneSplittersIfNeeded [

	(isCollapsed not
		and: [ self paneMorphs isNotEmpty and: [self splitters isEmpty]])
			ifTrue: [self addPaneSplitters]
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> addPaneVSplitterBetween: leftMorphs and: rightMorphs [
	"Add a vertical splitter for the given morphs that share a common right fraction.
	If there is a vertical discontinuity apply the splitter to the first contiguous group.
	Answer the morphs to which the splitter was applied."

	|targetX fixed bottomFraction topFrame bottomFrame sorted morph leftGroup rightGroup splitter offset|
	leftMorphs ifEmpty: [^self].
	targetX := leftMorphs first layoutFrame rightFraction.
	fixed := leftMorphs select: [:m | m layoutFrame leftFraction = m layoutFrame rightFraction].
		"fixed morphs appear in both top and bottom"
	sorted := ((leftMorphs reject: [:m | m layoutFrame leftFraction = m layoutFrame rightFraction])
		asSortedCollection: [:a :b | a layoutFrame bottomFraction = b layoutFrame bottomFraction
			ifTrue: [a layoutFrame topFraction <= b layoutFrame topFraction]
			ifFalse: [a layoutFrame bottomFraction <= b layoutFrame bottomFraction]]) readStream.
	sorted contents ifEmpty: [^fixed].
	leftGroup := OrderedCollection new.
	bottomFraction := sorted contents first layoutFrame topFraction.
	[sorted atEnd or: [morph := sorted next.
			morph layoutFrame topFraction ~= bottomFraction and: [
				morph layoutFrame bottomFraction ~= bottomFraction]]] whileFalse: [
		leftGroup add: morph.
		bottomFraction := morph layoutFrame bottomFraction].
	topFrame := leftGroup first layoutFrame.
	bottomFrame := leftGroup last layoutFrame.
	rightGroup := (rightMorphs
			reject: [:m | m layoutFrame leftFraction = m layoutFrame rightFraction])
			select: [:m |
		m layoutFrame topFraction
			between: topFrame topFraction
			and: bottomFrame bottomFraction].
	offset := (leftGroup collect: [:m | m layoutFrame rightOffset]) max.
	splitter := ProportionalSplitterMorph new.
	splitter layoutFrame: ((targetX @ topFrame topFraction corner: targetX @ bottomFrame bottomFraction) asLayoutFrame
								leftOffset: offset ;
								topOffset: topFrame topOffset ;
								rightOffset: 4 + offset;
								bottomOffset: bottomFrame bottomOffset).
	leftGroup := leftGroup, fixed.
	leftGroup do: [:m | splitter addLeftOrTop: m].
	rightGroup do: [:m | splitter addRightOrBottom: m].
	self addMorphBack: splitter.
	^leftGroup
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> addPaneVSplitters [
	"Add the vertical pane splitters."

	|remaining targetRight sameRight sameLeft |
	remaining := paneMorphs reject: [:each |
			each layoutFrame rightFraction = 1 or: [
				each layoutFrame rightFraction = 0]].
	[remaining notEmpty] whileTrue: [
		targetRight := remaining first layoutFrame rightFraction.
		sameRight := remaining select: [:each |
			each layoutFrame rightFraction = targetRight].
		sameLeft := paneMorphs select: [:each |
			each layoutFrame leftFraction = targetRight and: [
				each layoutFrame rightFraction ~= targetRight]].
		remaining := remaining
			copyWithoutAll: (self addPaneVSplitterBetween: sameRight and: sameLeft)]
]

{ #category : 'top window' }
SystemWindow >> adjustBorderUponActivationWhenLabeless [
	"Adjust the border upon, um, activation when, um, labelless"

	| aWidth |
	(aWidth := self valueOfProperty: #borderWidthWhenActive) ifNotNil:
		[self acquireBorderWidth: aWidth]
]

{ #category : 'top window' }
SystemWindow >> adjustBorderUponDeactivationWhenLabeless [
	"Adjust the border upon deactivation when, labelless"

	| aWidth |
	(aWidth := self valueOfProperty: #borderWidthWhenInactive) ifNotNil:
		[self acquireBorderWidth: aWidth]
]

{ #category : 'controls' }
SystemWindow >> allowedToClose [
	"Answer whether the window is currently allowed to close."

	^self mustNotClose not and: [
		self modalChild isNil]
]

{ #category : 'stepping' }
SystemWindow >> amendSteppingStatus [
	"Circumstances having changed, find out whether stepping is wanted and assure that the new policy is carried out"

	self wantsSteps
		ifTrue:
			[self arrangeToStartStepping]
		ifFalse:
			[self stopStepping]
]

{ #category : 'announcement' }
SystemWindow >> announce: anAnnouncement [
	"announcer is lazy initialized. If announcer is not set, this means that there is no listener, no need to propagate the announcement therefore"
	self doAnnounce: anAnnouncement
]

{ #category : 'announcement' }
SystemWindow >> announceActivated [

	self announce: (
		WindowActivated new
			window: self;
			yourself
	)
]

{ #category : 'announcement' }
SystemWindow >> announceDeActivated [

	self announce: (
		WindowDeActivated new
			window: self;
			yourself
	)
]

{ #category : 'announcement' }
SystemWindow >> announceOpened [

	super announceOpened.
	self currentWorld announcer
		announce: (WindowOpened new
						window: self;
						yourself)
]

{ #category : 'initialization' }
SystemWindow >> applyModelExtent [
	self extent: model initialExtent
]

{ #category : 'activation' }
SystemWindow >> basicActivate [
	"Bring me to the front and make me able to respond to mouse and keyboard."

	| outerMorph |

	outerMorph := self topRendererOrSelf.
	outerMorph owner ifNil: [^ self "avoid spurious activate when drop in trash"].
	self isTopWindow ifTrue: [^self].

	self beTopWindow.

	outerMorph owner firstSubmorph == outerMorph
		ifFalse: ["Bring me (with any flex) to the top if not already"
				outerMorph owner addMorphFront: outerMorph
		].

	self submorphsDo: [:m | m unlock].

	labelArea ifNotNil: [
		labelArea submorphsDo: [:m | m unlock].
		self setStripeColorsFrom: self paneColorToUse
	].

	self isCollapsed ifFalse: [
		self positionSubmorphs.
		labelArea ifNil: [ self adjustBorderUponActivationWhenLabeless ]
	].

	self privateFullBounds: nil; changed "ensure fullBounds computed for active drop shadow"
]

{ #category : 'testing' }
SystemWindow >> basicIsSticky [
	"Answer the super isSticky."

	^super isSticky
]

{ #category : 'accessing' }
SystemWindow >> basicLabel [
	"Answer the actual label morph."

	^label
]

{ #category : 'controls' }
SystemWindow >> beResizeable [

	isResizeable := true.

	"If isResizable is overriden to always return false, then it would break the invariant"
	self isResizeable ifTrue: [ self addGripsIfWanted ]
]

{ #category : 'activation' }
SystemWindow >> beTopWindow [

	| oldTopWindow |

	oldTopWindow := TopWindow.
	TopWindow := self.

	oldTopWindow ifNotNil: [ oldTopWindow privateBePassive ].

	self announceActivated
]

{ #category : 'controls' }
SystemWindow >> beUnresizeable [

	isResizeable := false.

	"If isResizable is overriden to always return true, then it would break the invariant"
	self isResizeable ifFalse: [ self removeGrips ]
]

{ #category : 'controls' }
SystemWindow >> beWithGrips [
	"Add the grips and set a property to
	indicate that grips are wanted."

	self removeProperty: #noGrips.
	(self isCollapsed not or: [self isTaskbarPresent]) ifTrue: [
		self addGripsIfWanted]
]

{ #category : 'controls' }
SystemWindow >> beWithoutGrips [
	"Remove the grips and set a property to
	indicate that grips are not wanted."

	self setProperty: #noGrips toValue: true.
	self removeGrips
]

{ #category : 'initialization' }
SystemWindow >> boxExtent [
	"answer the extent to use in all the buttons.
	The label height is used to be proportional to the standard window label font"

	label ifNil: [^(14 @ 14) * self displayScaleFactor].
	^ (((14 @ 14) * self displayScaleFactor) max: label height @ label height)
]

{ #category : 'activation' }
SystemWindow >> bringBehind: aMorph [
	"Make the receiver be directly behind the given morph.
	Take into account any modal owner and propagate."

	|outerMorph|
	outerMorph := self topRendererOrSelf.
	outerMorph owner ifNil: [^ self "avoid spurious activate when drop in trash"].
	outerMorph owner addMorph: outerMorph after: aMorph topRendererOrSelf.
	self modalOwner ifNotNil: [:mo | mo bringBehind: self]
]

{ #category : 'controls' }
SystemWindow >> buildWindowMenu [

	^ self menu
]

{ #category : 'testing' }
SystemWindow >> canBeMaximized [
	"Answer whether we are not we can be maximised."

	^ self isResizeable and: [ self isNotMaximized ]
]

{ #category : 'menu actions' }
SystemWindow >> changeColor [
	"Change the color of the receiver -- triggered, e.g. from a menu.  This variant allows the recolor triggered from the window's halo recolor handle to have the same result as choosing change-window-color from the window-title menu"
	self setWindowColor
]

{ #category : 'open/close' }
SystemWindow >> close [
	^ self delete
]

{ #category : 'controls' }
SystemWindow >> closeBoxHit [
	"The user clicked on the close-box control in the window title.
	For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down.
	If we have a modal child then don't delete."

	self allowedToClose ifFalse: [^self].
	self close
]

{ #category : 'keymapping' }
SystemWindow >> closeWindowAction [
"closed by a shortcut action.
 subclasses may overwrite this to do a different action (for example, cancel edits)"
	self close
]

{ #category : 'resize/collapse' }
SystemWindow >> collapse [
	self isCollapsed ifFalse:[self collapseOrExpand]
]

{ #category : 'controls' }
SystemWindow >> collapseBoxHit [
	"The user has clicked on the collapse box.
	Collapse or expand the receiver as appropriate."

	self collapseOrExpand
]

{ #category : 'resize/collapse' }
SystemWindow >> collapseOrExpand [

	"Collapse or expand the window, depending on existing state.
	Use the taskbar if present, otherwise do as normal."

	| cf |
	self isTaskbarPresent ifTrue: [ ^ self minimizeOrRestore ].
	isCollapsed
		ifTrue: [ "Expand -- restore panes to morphics structure"
			isCollapsed := false.
			self activate. "Bring to frint first"
			collapsedFrame := self getBoundsWithFlex.
			collapseBox ifNotNil: [
				collapseBox setBalloonText: 'Collapse this window' ].
			self setBoundsWithFlex: fullFrame.
			paneMorphs reverseDo: [ :m |
				self addMorph: m unlock.
				m startSteppingSubmorphs ].
			self
				addPaneSplitters;
				addGripsIfWanted ]
		ifFalse: [ "Collapse -- remove panes from morphics structure"
			isCollapsed := true.
			fullFrame := self getBoundsWithFlex.
			"First save latest fullFrame"
			paneMorphs do: [ :m |
				m
					delete;
					releaseCachedState ].
			self removePaneSplitters.
			self removeGrips.
			cf := self getCollapsedFrame.
			collapsedFrame ifNil: [ collapsedFrame := cf ].
			self setBoundsWithFlex: cf.
			collapseBox ifNotNil: [
				collapseBox setBalloonText: 'expand this window' ].
			expandBox ifNotNil: [
				expandBox setBalloonText: 'expand to full screen' ] ].
	self layoutChanged.

	"This gets invoked only if the taskbar is not present, if it is, then minimizeOrRestore is invoked (cf beginning of this method)"
	self announce: (WindowCollapsed new
			 window: self;
			 yourself)
]

{ #category : 'resize/collapse' }
SystemWindow >> collapsedFrame [
	^ collapsedFrame
]

{ #category : 'drawing' }
SystemWindow >> colorForInsets [
	^self paneColor colorForInsets
]

{ #category : 'embedding' }
SystemWindow >> configureForEmbedding [
	"Set up the window so it can be embedded into another morph."

	self detachKeymapCategory: #WindowShortcuts.
	labelArea owner ifNotNil: [
		self
			hasDropShadow: false;
			beWithoutGrips;
			removeLabelArea;
			makeBorderless.
		self submorphsDo: [ :m | m unlock ] ]
]

{ #category : 'embedding' }
SystemWindow >> configureForUnembedding [
	"Set up the window so it can be unembedded and placed in the world."

	labelArea owner ifNil: [
			self
				makeBordered;
				initializeLabelArea;
				themeChanged;
				addGrips ].
	self attachKeymapCategory: #WindowShortcuts.
]

{ #category : 'accessing' }
SystemWindow >> copyTitle [

	Clipboard clipboardText: self label.
	InformativeNotification signal: 'Window title copied.'
]

{ #category : 'initialization' }
SystemWindow >> createBox [
	"create a button with default to be used in the label area"
	"Transcript show: self paneColor asString;
	cr."
	| box |
	box := IconicButtonMorph new.
	box color: Color transparent;
		 target: self;
		 useSquareCorners;
		 borderWidth: 0.

	^ box
]

{ #category : 'controls' }
SystemWindow >> createCloseBox [
	"Answer a button for closing the window."

	^self theme createCloseBoxFor: self
]

{ #category : 'controls' }
SystemWindow >> createCollapseBox [
	"Answer a button for minimising the window."

	^self theme createCollapseBoxFor: self
]

{ #category : 'controls' }
SystemWindow >> createExpandBox [
	"Answer a button for maximising/restoring the window."

	^self theme createExpandBoxFor: self
]

{ #category : 'controls' }
SystemWindow >> createMenuBox [
	"Answer a button for the window menu."

	^self theme createMenuBoxFor: self
]

{ #category : 'menu actions' }
SystemWindow >> createWindowGroup [
	| group pos ext |
	pos := self position.
	ext := self extent.
	group := GroupWindowMorph new.
	group addWindow: self.
	(group openInWindowLabeled: 'Group: ' translated , self label)
		extent: ext;
		position: pos;
		model: group
]

{ #category : 'initialization' }
SystemWindow >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ #raised
]

{ #category : 'initialization' }
SystemWindow >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ self theme backgroundColor
]

{ #category : 'controls' }
SystemWindow >> defaultIsResizeable [

	^ true
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> delete [
	"Should activate window before asking model if okToChange
	since likely that a confirmation dialog will be requested.
	Don't if not owned by the world though."

	"in case we add some panes and reopen!"
	self isCloseable
		ifFalse: [ ^ self ].
	self deleteDiscardingChanges
]

{ #category : 'menu actions' }
SystemWindow >> deleteCloseBox [
	closeBox ifNotNil:
		[closeBox delete.
		closeBox := nil]
]

{ #category : 'open/close' }
SystemWindow >> deleteDiscardingChanges [

	| thisWorld announcement |
	self removePaneSplitters. "in case we add some panes and reopen!"
	thisWorld := self world.
	self isFlexed
		ifTrue: [ owner delete ]
		ifFalse: [ super delete ].
	model ifNotNil: [
		model
			windowIsClosing;
			releaseActionMap ].
	model := nil.
	SystemWindow noteTopWindowIn: thisWorld.

	announcement := WindowClosed new
		                window: self;
		                yourself.
	self announce: announcement.
	self useHideForClose ifFalse: [
		self currentWorld announcer announce: announcement ].
	
]

{ #category : 'menu building' }
SystemWindow >> discoveredMenuPragmaKeyword [

	^ #windowMenu
]

{ #category : 'printing' }
SystemWindow >> displayStringOn: aStream [

	label
		ifNotNil: [ aStream nextPutAll: self labelString ]
		ifNil: [ aStream nextPutAll: 'Untilted Window' ]
]

{ #category : 'events' }
SystemWindow >> doFastFrameDrag: grabPoint [
	"Do fast frame dragging from the given point"

	(FastDraggingFrameMorph forDragging: self clickedAt: grabPoint) openInWorld
]

{ #category : 'resize/collapse' }
SystemWindow >> doFastWindowReframe: aSideOrCornerName [
	"Do fast frame resizing  from the given corner or side"

	(FastDraggingFrameMorph forResizing: self fromLocation: aSideOrCornerName) openInWorld
]

{ #category : 'events' }
SystemWindow >> doubleClick: event [
	"Handle a double click. Maximize/restore the window.
	Works in title bar area."

	self isResizeable ifFalse: [ ^ self ].

	(labelArea containsPoint: event position)
		ifTrue: [self expandBoxHit]
]

{ #category : 'events' }
SystemWindow >> doubleClickTimeout: event [
	"Forget the #inactiveDoubleClick property.
	The property is set if an inactive window was double-clicked."

 	self removeProperty: #inactiveDoubleClick
]

{ #category : 'drawing' }
SystemWindow >> drawDropShadowOn: aCanvas [
	"Get the theme to draw the drop shawdow for the receiver."

	|dropAreas|
	dropAreas := self areasRemainingToFill: (self bounds expandBy: self shadowMargins).
	(dropAreas anySatisfy: [:rect | aCanvas isVisible: rect])
		ifFalse: [^self]. "no need to draw since no intersection"
	self isActive
		ifTrue: [self theme
					drawWindowActiveDropShadowFor: self
					on: aCanvas]
		ifFalse: [self theme
					drawWindowInactiveDropShadowFor: self
					on: aCanvas]
]

{ #category : 'accessing' }
SystemWindow >> embeddable [
	^ embeddable ifNil: [embeddable := false]
]

{ #category : 'accessing' }
SystemWindow >> embeddable: aBoolean [
	embeddable := aBoolean
]

{ #category : 'accessing' }
SystemWindow >> embeddedWindowOrNil [

	"answer nil for common morphs, yourself from system windows and first submorph for transformation morphs"

	^ self
]

{ #category : 'panes' }
SystemWindow >> existingPaneColor [
	"Answer the existing pane color for the window, obtaining it from the first paneMorph if any, and fall back on using the second stripe color if necessary."

	| aColor |
	aColor := self valueOfProperty: #paneColor.
	aColor ifNil: [self setProperty: #paneColor toValue: (aColor := self paneColor)].
	^aColor
]

{ #category : 'resize/collapse' }
SystemWindow >> expand [
	self isCollapsed ifTrue:[self collapseOrExpand]
]

{ #category : 'controls' }
SystemWindow >> expandBoxHit [
	"The fullscreen expand box has been hit"

	self isCollapsed ifTrue: [
			self
				hide;
				collapseOrExpand.
			self unexpandedFrame ifNil: [ self unexpandedFrame: fullFrame ].
			self
				fullscreen;
				setExpandBoxBalloonText.
			^ self show ].
	self unexpandedFrame
		ifNil: [
				"Some window should not be able to be maximized so we check it there."
				self canBeMaximized ifFalse: [ ^ self ].
				self
					unexpandedFrame: fullFrame;
					fullscreen ]
		ifNotNil: [
				self
					bounds: self unexpandedFrame;
					unexpandedFrame: nil ].
	self setExpandBoxBalloonText
]

{ #category : 'resize/collapse' }
SystemWindow >> extent: aPoint [
	"Set the receiver's extent to value provided. Respect my minimumExtent."

	| newExtent w oldExtent|
	newExtent := self isCollapsed
		ifTrue: [aPoint max: (self labelWidgetAllowance @ 0)]
		ifFalse: [aPoint max: self minimumExtent].
	newExtent = self extent ifTrue: [^ self].

	oldExtent := self extent.
	isCollapsed
		ifTrue: [super extent: newExtent x @ self labelHeight]
		ifFalse: [super extent: newExtent].
	isCollapsed
		ifTrue: [collapsedFrame := self bounds]
		ifFalse: [fullFrame := self bounds].
	(self isCollapsed or: [label isNil]) "shrink the label if insufficient space"
		ifFalse: [
				label minWidth: nil.
				label fitContents.

				"Add a tooltip on the label with the full content in case there is not enough space"
				(label width > (bounds width - labelWidgetAllowance))
					ifTrue: [ label setBalloonText: label contents ]
					ifFalse: [ label setBalloonText: nil ].

				w := (label width min: bounds width - labelWidgetAllowance).
				label setWidth: w; minWidth: w.
				label align: label bounds topCenter with: bounds topCenter + (0@borderWidth).
				collapsedFrame ifNotNil:
					[collapsedFrame := collapsedFrame withWidth: label width + labelWidgetAllowance]].
	self theme windowExtentChangedFor: self.

	self announce: (WindowResizing new
						oldSize: oldExtent;
						newSize: newExtent;
						window: self;
						yourself)
]

{ #category : 'accessing' }
SystemWindow >> externalName [
	"Answer the name by which the receiver is known in the UI"

	^ labelString
]

{ #category : 'menu building' }
SystemWindow >> fallbackMenuOn: menu [
	"Build the menu that is put up if something is going wrong with the menubuilder"

	menu
		addToggle: 'Close' translated
		target: self
		selector: #closeBoxHit
		getStateSelector: nil
		enablementSelector: #allowedToClose.
	menu lastItem icon: self theme windowCloseForm
]

{ #category : 'resize/collapse' }
SystemWindow >> fastFramingOn [

	^self theme settings fastDragging and: [self isFlexed not]
]

{ #category : 'accessing' }
SystemWindow >> fillStyleToUse [
	"Answer the basic fill style for the receiver."

	^self isActive
		ifTrue: [self activeFillStyle]
		ifFalse: [self inactiveFillStyle]
]

{ #category : 'resize/collapse' }
SystemWindow >> fullFrame [
	^ fullFrame
]

{ #category : 'controls' }
SystemWindow >> fullscreen [
	"Zoom Window to Full World size with possible DeskMargins"
	"SystemWindow fullscreen"

	self bounds: self fullscreenBounds
]

{ #category : 'menu actions' }
SystemWindow >> fullscreenBounds [
	"Answer the bounds that the receiver would take if expanded to fullscreen."

	^ (RealEstateAgent maximumUsableAreaInWorld: self world) insetBy: self fullscreenMargin
]

{ #category : 'settings' }
SystemWindow >> fullscreenMargin [
	^ self class fullscreenMargin
]

{ #category : 'resize/collapse' }
SystemWindow >> getBoundsWithFlex [
	"Return the lastest bounds rectangle with origin forced to global coordinates"

	^ self isFlexed
		ifTrue: [ (owner transform localPointToGlobal: bounds topLeft) extent: bounds extent ]
		ifFalse: [ self bounds ]
]

{ #category : 'resize/collapse' }
SystemWindow >> getCollapsedFrame [

	^RealEstateAgent assignCollapseFrameFor: self
]

{ #category : 'accessing' }
SystemWindow >> getRawLabel [
	"Answer a shallowCopy of the label with the contents fitted."

	|contentsFit|
	contentsFit := label shallowCopy fitContents.
	contentsFit extent: (label extent x min: contentsFit extent x) @ contentsFit extent y.
	^contentsFit
]

{ #category : 'events' }
SystemWindow >> grabSelfOrTopRenderer: evt [

	| renderer |

	renderer := self topRendererOrSelf.

	evt hand grabMorph: renderer.

	(renderer isKindOf: SystemWindow) ifTrue: [
		renderer position: evt hand position + (renderer position - evt startPoint)
	]
]

{ #category : 'initialization' }
SystemWindow >> gradientWithColor: aColor [

	| ramp |
	ramp := {0.0 -> Color white. 1.0 -> aColor}.

	^ (GradientFillStyle ramp: ramp)
		radial: true;
		origin: self bounds origin;
		direction: 0 @ 223;
		normal: 223 @ 0
]

{ #category : 'events' }
SystemWindow >> handleListenEvent: evt [
	"Make sure we lock our contents after DnD has finished"
	evt isMouse ifFalse:[^self].
	evt hand hasSubmorphs ifTrue:[^self]. "still dragging"
	self isTopWindow ifFalse:[self lockInactivePortions].
	evt hand removeMouseListener: self
]

{ #category : 'events' }
SystemWindow >> handlesDropShadowInHand [
	"Answer whether the receiver will handle drop shadow drawing when picked up in the hand."

	^self theme handlesWindowDropShadowInHandFor: self
]

{ #category : 'events' }
SystemWindow >> handlesKeyboard: evt [
	"Return true if the receiver wishes to handle the given keyboard event"

	(super handlesKeyboard: evt) ifTrue: [^true].
	^evt anyModifierKeyPressed and: [
		evt keyCharacter = Character arrowLeft or: [
		evt keyCharacter = Character arrowRight or: [
		evt keyCharacter = Character delete or: [
		evt keyCharacter = $w or: [
		evt keyCharacter = Character tab ]]]]]
]

{ #category : 'events' }
SystemWindow >> handlesMouseDown: evt [
	"If I am not the topWindow, then I will only respond to dragging by the title bar.
	Any other click will only bring me to the top"

	^ self isEmbedded not
]

{ #category : 'events' }
SystemWindow >> handlesMouseOverDragging: evt [
	^true
]

{ #category : 'controls' }
SystemWindow >> hasCloseBox [
	"Answer whether the receiver currently has a close box."

	^ closeBox isNotNil
]

{ #category : 'controls' }
SystemWindow >> hasCollapseBox [
	"Answer whether the receiver currently has a collapse box."

	^ collapseBox isNotNil
]

{ #category : 'controls' }
SystemWindow >> hasExpandBox [
	"Answer whether the receiver currently has an expand box."

	^ expandBox isNotNil
]

{ #category : 'controls' }
SystemWindow >> hasMenuBox [
	"Answer whether the receiver currently has a menu box."

	^ menuBox isNotNil
]

{ #category : 'compatibility' }
SystemWindow >> hasUnacceptedEdits [

	^ (self submorphs select: [:e | e respondsTo: #hasUnacceptedEdits ]) anySatisfy: [:e | e hasUnacceptedEdits ]
]

{ #category : 'thumbnail' }
SystemWindow >> icon [
	"Answer a form with an icon to represent the receiver"

	^ self iconNamed: #window
]

{ #category : 'accessing' }
SystemWindow >> inactiveFillStyle [
	"Return the active fillStyle for the receiver."

	^self theme windowInactiveFillStyleFor: self
]

{ #category : 'accessing' }
SystemWindow >> inactiveLabelFillStyle [
	"Return the inactive label fillStyle for the receiver."

	^self theme windowInactiveLabelFillStyleFor: self
]

{ #category : 'accessing' }
SystemWindow >> inactiveTitleFillStyle [
	"Return the inactive title fillStyle for the receiver."

	^self theme windowInactiveTitleFillStyleFor: self
]

{ #category : 'accessing' }
SystemWindow >> indicateModalChild [
	"Make the user aware that this is the topmost modal child
	by flashing."

	(self isMinimized and: [self isTaskbarPresent])
		ifTrue: [self worldTaskbar ifNotNil: [:tb |
					tb indicateModalChildForMorph: self]]
		ifFalse: [self flash]
]

{ #category : 'open/close' }
SystemWindow >> initialExtent [
	^ (self model respondsTo: #initialExtent)
		ifTrue: [self model initialExtent]
		ifFalse: [ self extent ]
]

{ #category : 'initialization' }
SystemWindow >> initialize [
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [labelString := 'Untitled Window'].
	isCollapsed := false.
	paneMorphs := Array new.
	self layoutPolicy: ProportionalLayout new.
	self clipSubmorphs: true.
	self theme
		configureWindowBorderFor: self;
		configureWindowDropShadowFor: self.
	self initializeLabelArea.

	self cellPositioning: #topLeft. "make the offsets easy to calculate!"
	self addGripsIfWanted.

	self extent: (300 @ 200) scaledByDisplayScaleFactor.
	mustNotClose := false.
	updatablePanes := Array new.
	self bindKeyCombination: ($k meta shift alt ) toAction: [self taskbarMoveLeft ].
	self bindKeyCombination: ($l meta shift alt) toAction: [self taskbarMoveRight ].
	self bindKeyCombination: $w meta toAction: [ self class closeTopWindow ]
]

{ #category : 'initialization' }
SystemWindow >> initializeLabelArea [
	"Initialize the label area (titlebar) for the window."

	label := self theme windowLabelFor: self.
	"Add default inital boxes"
	collapseBox := self createCollapseBox. "Add collapse box so #labelHeight will work"
	closeBox := self createCloseBox.
	self wantsExpandBox ifTrue: [
		expandBox := self createExpandBox.
		self setExpandBoxBalloonText].
	menuBox := self createMenuBox.
	stripes := {Morph newBounds: bounds . Morph newBounds: bounds }.
	self addLabelArea.
	labelArea
		goBehind.
	self replaceBoxes.
	labelArea fillStyle: self activeTitleFillStyle
]

{ #category : 'keymapping' }
SystemWindow >> initializeShortcuts: aKMDispatcher [
	"Where we may attach keymaps or even on:do: local shortcuts if needed."

	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #WindowShortcuts
]

{ #category : 'activation' }
SystemWindow >> isActive [
	"Answer active if no owner too to avoid color flickering."

	^self isWindowActive: self
]

{ #category : 'testing' }
SystemWindow >> isCloseable [
	^ self mustNotClose not and: [ model ifNotNil: [ model okToChange ] ifNil: [ true ] ]
]

{ #category : 'resize/collapse' }
SystemWindow >> isCollapsed [
	^ isCollapsed
]

{ #category : 'resize/collapse' }
SystemWindow >> isCollapsed: anObject [

	isCollapsed := anObject
]

{ #category : 'testing' }
SystemWindow >> isDisplayed [
	"Answer true if I am currently displayed in the World"

	^ self world isNotNil
]

{ #category : 'embedding' }
SystemWindow >> isEmbedded [
	"Answer whether the receiver is embedded, i.e.
	has an owner that is not the world or the hand."

	^self owner isNotNil and: [self owner ~= self world and: [self owner ~= self activeHand]]
]

{ #category : 'testing' }
SystemWindow >> isMaximized [
	"Answer whether we are maximised."

	^ self unexpandedFrame isNotNil
]

{ #category : 'testing' }
SystemWindow >> isMinimized [
	"Answer whether we are minimised."

	^self isCollapsed
]

{ #category : 'testing' }
SystemWindow >> isNotMaximized [
	"Answer whether we are not maximised."

	^self unexpandedFrame isNil
]

{ #category : 'testing' }
SystemWindow >> isNotMinimized [
	"Answer whether we are not minimised."

	^self isCollapsed not
]

{ #category : 'testing' }
SystemWindow >> isNotRestored [
	"Answer whether we are maximised or minimised."

	^self isMinimized or: [self isMaximized]
]

{ #category : 'controls' }
SystemWindow >> isResizeable [
	"Answer whether we are not we can be resized."

	^ isResizeable ifNil: [ isResizeable := self defaultIsResizeable ]
]

{ #category : 'controls' }
SystemWindow >> isResizeable: aBoolean [

	aBoolean = isResizeable ifTrue: [ ^ self ].
	aBoolean
		ifTrue: [ self beResizeable ]
		ifFalse: [ self beUnresizeable ]
]

{ #category : 'testing' }
SystemWindow >> isRestored [
	"Answer whether we are neither expanded or collapsed."

	^(self isMinimized or: [self isMaximized]) not
]

{ #category : 'testing' }
SystemWindow >> isSystemWindow [
"answer whatever the receiver is a SystemWindow"
	^ true
]

{ #category : 'top window' }
SystemWindow >> isTopWindow [

	^self == TopWindow
]

{ #category : 'events' }
SystemWindow >> justDroppedInto: aMorph event: anEvent [
	"Release the mouse focus as well."

	isCollapsed
		ifTrue: [
			self position: ((self position max: 0@0) grid: 8@8).
			collapsedFrame := self bounds
		]
		ifFalse: [
			fullFrame := self bounds.
			self isTopWindow ifFalse: [self activate]
		].

	anEvent hand releaseMouseFocus.

	^super justDroppedInto: aMorph event: anEvent
]

{ #category : 'events' }
SystemWindow >> keyDown: evt [
	"Check for close window."
	super keyDown: evt.
	(self navigationKey: evt) ifTrue: [^true].
	^false
]

{ #category : 'accessing' }
SystemWindow >> label [
	^ labelString
]

{ #category : 'accessing' }
SystemWindow >> labelArea [
	"Answer the label area."

	^labelArea
]

{ #category : 'accessing' }
SystemWindow >> labelHeight [
	"Answer the height for the window label.  The standard behavior is at bottom; a hook is provided so that models can stipulate other heights, in support of various less-window-looking demos.
	If no label answer the class border width instead."

	label ifNil: [^self isEmbedded ifTrue: [0] ifFalse: [self class borderWidth]].
	^(label height + (self class borderWidth * 2)) max:
		(collapseBox ifNotNil: [collapseBox height] ifNil: [10])
]

{ #category : 'geometry' }
SystemWindow >> labelRect [
	^ self innerBounds withHeight: self labelHeight
]

{ #category : 'accessing' }
SystemWindow >> labelString [

	"Answer the actual label string."

	^ label ifNil: [ labelString ] ifNotNil: [ label contents asString ]
]

{ #category : 'accessing' }
SystemWindow >> labelWidgetAllowance [
	^ labelWidgetAllowance ifNil: [self setLabelWidgetAllowance]
]

{ #category : 'layout' }
SystemWindow >> layoutBounds [
	"Bounds of pane area only."
	| box |

	box := super layoutBounds.
	^box withTop: box top + self labelHeight
]

{ #category : 'layout' }
SystemWindow >> layoutChanged [
	"No need to propagate to the world.
	Fixed to always flush layout cache."

	(self owner isNil or: [self owner isWorldMorph not])
		ifTrue: [^super layoutChanged].
	fullBounds := nil.
	self layoutPolicy ifNotNil: [:layout | layout flushLayoutCache]
]

{ #category : 'activation' }
SystemWindow >> linkSplittersToSplitters [
	"The pane morphs are already linked. Cross link the splitters as appropriate."

	self splitters do: [:each |
		each splitsTopAndBottom
			ifTrue: [self splitters do: [:eachMorph |
					eachMorph splitsTopAndBottom ~= each splitsTopAndBottom ifTrue: [
						eachMorph layoutFrame bottomFraction = each layoutFrame topFraction
							ifTrue: [each addLeftOrTop: eachMorph].
						eachMorph layoutFrame topFraction = each layoutFrame bottomFraction
							ifTrue: [each addRightOrBottom: eachMorph]]]]
			ifFalse: [self splitters do: [:eachMorph |
					eachMorph splitsTopAndBottom ~= each splitsTopAndBottom ifTrue: [
						eachMorph layoutFrame rightFraction = each layoutFrame leftFraction
							ifTrue: [each addLeftOrTop: eachMorph].
						eachMorph layoutFrame leftFraction = each layoutFrame rightFraction
							ifTrue: [each addRightOrBottom: eachMorph]]]]]
]

{ #category : 'activation' }
SystemWindow >> lockInactivePortions [
	"Make me unable to respond to mouse and keyboard.  Control boxes remain active."

	self isEmbedded ifTrue: [^self].
	self submorphsDo: [:m | m == labelArea ifFalse: [m lock]]
]

{ #category : 'controls' }
SystemWindow >> makeBordered [
	"Add the border and border width offsets."

	|b|
	self borderWidth = 0 ifFalse: [^self].
	b  := self class borderWidth.
	self submorphsDo: [:m | | l |
		l := m layoutFrame.
		l ifNotNil: [
			l rightFraction = 1 ifTrue: [l rightOffset: l rightOffset - b].
			l leftFraction = 0 ifTrue: [l leftOffset: l leftOffset + b].
			l bottomFraction = 1 ifTrue: [l bottomOffset: l bottomOffset - b]]].
	self theme configureWindowBorderFor: self
]

{ #category : 'controls' }
SystemWindow >> makeBorderless [
	"Remove the border and border width offsets.."

	|b|
	b  := self class borderWidth.
	self submorphsDo: [:m | | l |
		l := m layoutFrame.
		l ifNotNil: [
			l rightFraction = 1 ifTrue: [l rightOffset: l rightOffset + b].
			l leftFraction = 0 ifTrue: [l leftOffset: l leftOffset - b].
			l bottomFraction = 1 ifTrue: [l bottomOffset: l bottomOffset + b]]].
	self borderWidth: 0
]

{ #category : 'controls' }
SystemWindow >> makeClosable [
	"Reinstate the close box. Go via theme to maintain box order."

	mustNotClose := false.
	closeBox
		ifNil: [closeBox := self createCloseBox.
				self replaceBoxes]
]

{ #category : 'drawing' }
SystemWindow >> makeMeVisible [

	self world extent > (0@0) ifFalse: [^ self].

	((self world bounds insetBy: (0@0 corner: self labelHeight asPoint))
		containsPoint: self position) ifTrue: [^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self isCollapsed
		ifTrue: [self position: (RealEstateAgent assignCollapsePointFor: self)]
		ifFalse: [self position: (RealEstateAgent initialFrameFor: self initialExtent: self extent world: self world) topLeft]
]

{ #category : 'menu actions' }
SystemWindow >> makeSecondTopmost [
	| aWorld |
	aWorld := self world.
	aWorld submorphs
		detect: [ :m | m isSystemWindow and: [ m ~~ self ] ]
		ifFound: [ :nextWindow |
			nextWindow activate.
			aWorld addMorph: self behind: nextWindow ]
]

{ #category : 'menu actions' }
SystemWindow >> makeUnclosable [
	mustNotClose := true.
	self deleteCloseBox
]

{ #category : 'controls' }
SystemWindow >> maximize [
	"Maximise the receiver. If collapsed the uncollapse first."

	self isMinimized ifTrue: [self collapseOrExpand].
	self isMaximized ifFalse: [self expandBoxHit]
]

{ #category : 'initialization' }
SystemWindow >> maximumExtent [
	"This returns the maximum extent that the morph may be expanded to.
	Return nil if this property has not been set."

	^ self valueOfProperty: #maximumExtent
]

{ #category : 'initialization' }
SystemWindow >> maximumExtent: aPoint [
	"This returns the maximum extent that the morph may be expanded to.
	Return nil if this property has not been set."

	^ self setProperty: #maximumExtent toValue: aPoint
]

{ #category : 'api' }
SystemWindow >> menu [

	^(PragmaMenuBuilder
		pragmaKeyword: self discoveredMenuPragmaKeyword
		model: self) menu
]

{ #category : 'menu building' }
SystemWindow >> menuBox [
	"Answer the receiver's menu box."

	^menuBox
]

{ #category : 'controls' }
SystemWindow >> minimize [
	"Minimise the receiver."

	self isMinimized ifFalse: [self collapseBoxHit]
]

{ #category : 'resize/collapse' }
SystemWindow >> minimizeOrRestore [

	"Collapse or expand the window, depending on existing state"

	| mc windowEvent |
	isCollapsed
		ifTrue: [ "Expand -- restore panes to morphics structure"
			isCollapsed := false.
			"Bring to front first"
			self
				setBoundsWithFlex: fullFrame;
				comeToFront;
				show.
			mc := self modalChild.
			paneMorphs reverseDo: [ :m |
				mc ifNil: [ m unlock ].
				self addMorph: m.
				m startSteppingSubmorphs ].
			self activate ]
		ifFalse: [ "Collapse -- remove panes from morphics structure"
			isCollapsed := true.
			fullFrame := self getBoundsWithFlex.
			"First save latest fullFrame"
			paneMorphs do: [ :m |
				m
					delete;
					releaseCachedState ].
			self
				setBoundsWithFlex: (-100 @ -100 extent: 2 @ 2);
				"place offscreen"hide.
			self isActive ifTrue: [ self world navigateVisibleWindowForward ] ].
	self layoutChanged.

	"This gets invoked only if the taskbar is present, see sender of minimizeOrRestore"
	windowEvent := self isMinimized
		               ifTrue: [ WindowCollapsed new ]
		               ifFalse: [ WindowExpanded new ].

	windowEvent window: self.
	self announce: windowEvent
]

{ #category : 'modal-windows' }
SystemWindow >> modalChild [
	"Answer the modal child of the receiver, if any."

	^self valueOfProperty: #modalChild
]

{ #category : 'modal-windows' }
SystemWindow >> modalLockTo: aSystemWindow [
	"Lock the receiver as a modal owner of the given window."

	aSystemWindow setProperty: #modalOwner toValue: self.
	self setProperty: #modalChild toValue: aSystemWindow.

	closeBox ifNotNil: [
		self setProperty: #preModalCloseEnabled toValue: closeBox enabled.
		closeBox enabled: false
	]
]

{ #category : 'modal-windows' }
SystemWindow >> modalOwner [
	"Answer the modal owner of the receiver, if any."

	^self valueOfProperty: #modalOwner
]

{ #category : 'modal-windows' }
SystemWindow >> modalUnlockFrom: aSystemWindow [
	"Unlock the receiver as a modal owner of the given window."

	aSystemWindow removeProperty: #modalOwner.
	self removeProperty: #modalChild.
	closeBox ifNotNil: [:cl | cl enabled: (self valueOfProperty: #preModalCloseEnabled ifAbsent: [true])].
	self removeProperty: #preModalCloseEnabled.
	self activate
]

{ #category : 'accessing' }
SystemWindow >> model: anObject [
	"Set the model."

	super model: anObject.
	self
		setProperty: #paneColor toValue: self defaultBackgroundColor;
		fillStyle: self fillStyleToUse;
		setStripeColorsFrom: self paneColorToUse.
	self theme fadedBackgroundWindows ifFalse: [ "since not done in stripes" self adoptPaneColor: self paneColor ].
	self minimumExtent: ((anObject respondsTo: #minimumExtent) ifTrue: [ anObject minimumExtent ]).
	menuBox ifNotNil: [
			menuBox
				labelGraphic: (self theme windowMenuIconFor: self);
				height: self boxExtent y ]
]

{ #category : 'events' }
SystemWindow >> mouseDown: evt [
	"Changed to properly process the mouse down event if passing to
	submorphs."

	self isEmbedded ifTrue: [^self].

	self isTopWindow
		ifTrue: [self comeToFront] "rise above non-window morphs"
		ifFalse:[	self activate].

	evt hand waitForClicksOrDrag: self event: evt.  "allow double-click/drag response"

	evt wasHandled: false.
	self submorphsDo: [ :morph | "but allow a submorph to process and override double-click/drag response."
		(morph containsPoint: evt cursorPoint) ifTrue: [ morph processEvent: evt ]
	].
	evt wasHandled: true
]

{ #category : 'events' }
SystemWindow >> mouseEnterDragging: evt [
	"unlock children for drop operations"
	(self isTopWindow not and:[evt hand hasSubmorphs]) ifTrue:[
		self submorphsDo:[:m| m unlock].
		evt hand addMouseListener: self. "for drop completion on submorph"
	]
]

{ #category : 'events' }
SystemWindow >> mouseLeaveDragging: evt [
	"lock children after drop operations"
	(self isTopWindow not and:[evt hand hasSubmorphs]) ifTrue:[
		self lockInactivePortions.
		evt hand removeMouseListener: self.
	]
]

{ #category : 'events' }
SystemWindow >> mouseMove: evt [
	"Handle a mouse-move event"

	self basicIsSticky ifFalse:[
		self fastFramingOn
			ifTrue: [self doFastFrameDrag: evt startPoint ]
			ifFalse: [ self grabSelfOrTopRenderer: evt	]
	]
]

{ #category : 'events' }
SystemWindow >> mouseUp: evt [
	| cp |
	cp := evt cursorPoint.
	submorphs do: [:m | (m containsPoint: cp) ifTrue: [m mouseUp: evt]]
]

{ #category : 'move' }
SystemWindow >> moveTaskbarTaskToOffset: offset [

	self worldTaskbar ifNotNil: [ :worldTaskbar | worldTaskbar updateTasks; move: self taskbarTask withOffset: offset ]
]

{ #category : 'open/close' }
SystemWindow >> mustNotClose [
	^ mustNotClose == true
]

{ #category : 'activation' }
SystemWindow >> navigateFocus [

	self rememberedKeyboardFocus
		ifNil: [self navigateFocusForward]
		ifNotNil: [ :keyboardFocus |
			keyboardFocus world
				"ifNil: [self rememberKeyboardFocus: nil]" "deleted"
				ifNotNil: [
					keyboardFocus wantsKeyboardFocus
						ifTrue: [keyboardFocus takeKeyboardFocus]
						ifFalse: [self navigateFocusForward]
					]
		]
]

{ #category : 'navigation' }
SystemWindow >> navigateFocusForward [
	"Change the keyboard focus to the next morph or the receiver
	in none are interested."

	self nextMorphWantingFocus
		ifNil: [self takeKeyboardFocus]
		ifNotNil: [super navigateFocusForward]
]

{ #category : 'navigation' }
SystemWindow >> navigationKey: event [
	"Check for tab key activity and change focus as appropriate.
	Check for menu key to do popup.
	Check for active window naviagation."

	(self world navigationKey: event) ifTrue: [^true].
	(self tabKey: event) ifTrue: [^true].
	(event keyCharacter = Character escape and: [
			event anyModifierKeyPressed]) ifTrue: [
		^ self yellowButtonActivity: false].
	^false
]

{ #category : 'navigation' }
SystemWindow >> nextMorphAcrossInWindow [
	"Answer the next morph in the window. Traverse
	from the receiver to its next sibling or owner's next sibling etc.
	Make sure we behave like a normal Morph, if we aren't a toplevel window ( or not visible) "

	^ (self owner isNotNil and: [ self owner isWorldMorph ])
		ifTrue: [ self ]
		ifFalse: [ super nextMorphAcrossInWindow ]
]

{ #category : 'navigation' }
SystemWindow >> nextMorphInWindow [
	"Answer the next morph in the window. Traverse
	from the receiver to its first pane."

	^self hasSubmorphs
		ifTrue: [self submorphs first]
]

{ #category : 'menu actions' }
SystemWindow >> offerWindowMenu [
	| aMenu |
	aMenu := self buildWindowMenu.
	model ifNotNil:
		[model addModelItemsToWindowMenu: aMenu].
	aMenu popUpEvent: self currentEvent in: self world
]

{ #category : 'menu actions' }
SystemWindow >> openAsExternalWindow [

	self submorphs last openInExternalWindow.
	self delete.
]

{ #category : 'open/close' }
SystemWindow >> openModal [
	self openInWorld.
	MorphicRenderLoop new doOneCycleWhile: [ self isInWorld ]
]

{ #category : 'open/close' }
SystemWindow >> openModal: aSystemWindow [
	"Open the given window locking the receiver until it is dismissed.
	Answer the system window.
	Restore the original keyboard focus when closed."

	| keyboardFocus |

	keyboardFocus := self activeHand keyboardFocus.
	self modalLockTo: aSystemWindow.
	[ aSystemWindow openModal ] ensure: [
		self modalUnlockFrom: aSystemWindow.
		self activeHand newKeyboardFocus: keyboardFocus ].
	^aSystemWindow
]

{ #category : 'accessing' }
SystemWindow >> paneColor [

	"Answer the basic pane color that should be used."

	^self theme paneColorFor: self
]

{ #category : 'panes' }
SystemWindow >> paneColor: aColor [
	self setProperty: #paneColor toValue: aColor.

	self adoptPaneColor: aColor
]

{ #category : 'accessing' }
SystemWindow >> paneColorOrNil [
	"Answer the window's pane color or nil otherwise."

	^self paneColor
]

{ #category : 'panes' }
SystemWindow >> paneColorToUse [

	^ self paneColor
]

{ #category : 'panes' }
SystemWindow >> paneMorphSatisfying: aBlock [
	^ paneMorphs detect: aBlock ifNone: [ nil ]
]

{ #category : 'geometry' }
SystemWindow >> paneMorphs [
	"Nominally private but a need for obtaining this from the outside arose"

	^ paneMorphs
]

{ #category : 'resize/collapse' }
SystemWindow >> paneWithLongestSide: sideBlock near: aPoint [
	| thePane theSide theLen |
	theLen := 0.
	paneMorphs do:
		[:pane | | box | box := pane bounds.
		box forPoint: aPoint closestSideDistLen:
			[:side :dist :len |
			(dist <= 5 and: [len > theLen]) ifTrue:
				[thePane := pane.
				theSide := side.
				theLen := len]]].
	sideBlock value: theSide.
	^ thePane
]

{ #category : 'geometry' }
SystemWindow >> panelRect [
	"Answer the area below the title bar which is devoted to panes."

	^ self innerBounds insetBy: (0 @ self labelHeight corner: 0 @ 0)
]

{ #category : 'activation' }
SystemWindow >> passivate [
	"Make me unable to respond to mouse and keyboard"

	super passivate.
	self setStripeColorsFrom: self paneColorToUse.
	self isEmbedded ifTrue: [^self].
	"Control boxes remain active, except in novice mode"
	self lockInactivePortions.
	labelArea ifNil: "i.e. label area is nil, so we're titleless"
		[self adjustBorderUponDeactivationWhenLabeless]
]

{ #category : 'geometry' }
SystemWindow >> position: newPos [
	| oldPos |
	isCollapsed
		ifTrue: [ oldPos := collapsedFrame origin ]
		ifFalse: [ oldPos := fullFrame origin ].

	super position: newPos.
	isCollapsed
		ifTrue: [collapsedFrame := self bounds]
		ifFalse: [fullFrame := self bounds].

	(oldPos ~= newPos)
		ifTrue: [ self announce:
					(WindowMoved new
						oldPosition: oldPos;
						newPosition: newPos;
						window: self)]
]

{ #category : 'activation' }
SystemWindow >> positionModalOwner [

	self modalOwner ifNotNil: [ :modalOwner |
		(modalOwner isKindOf: SystemWindow) ifTrue: [
			modalOwner bringBehind: self.
		]
   ]
]

{ #category : 'open/close' }
SystemWindow >> positionSubmorphs [
	"Feels like overkill, but effect needed"
	super positionSubmorphs.
	self submorphsDo:
		[:aMorph | aMorph positionSubmorphs]
]

{ #category : 'opening' }
SystemWindow >> postOpenInWorld: aWorld [

	self activate.
	super postOpenInWorld: aWorld
]

{ #category : 'opening' }
SystemWindow >> preOpenInWorld: aWorld [
	"This msg and its callees result in the window being activeOnlyOnTop"

	self bounds: (RealEstateAgent initialFrameFor: self initialExtent: self initialExtent world: aWorld)
]

{ #category : 'theme' }
SystemWindow >> preferredCornerStyle [
	"Answer the preferred corner style."

	^self theme windowPreferredCornerStyleFor: self
]

{ #category : 'navigation' }
SystemWindow >> previousMorphInWindow [
	"Answer the previous morph in the window. This will be the
	last submorph recursively of the first pane morph."

	^self hasSubmorphs
		ifTrue: [self lastSubmorphRecursive]
]

{ #category : 'activation' }
SystemWindow >> privateBePassive [
	"private"
	self isInWorld ifFalse: [ ^ self ].

	self
		rememberKeyboardFocus: self activeHand keyboardFocus;
		passivate;
		announceDeActivated
]

{ #category : 'layout' }
SystemWindow >> putLabelItemsInLabelArea [
	"Put label items into the label area, if there is one"

	labelArea ifNotNil:
		[stripes ifNotNil: [stripes do: [:stripe | labelArea addMorph: stripe]].
		closeBox ifNotNil: [labelArea addMorph: closeBox].
		menuBox ifNotNil: [labelArea addMorph: menuBox].
		collapseBox ifNotNil: [labelArea addMorph: collapseBox].
		label ifNotNil: [labelArea addMorph: label]]
]

{ #category : 'drawing' }
SystemWindow >> raisedColor [
	^self paneColor raisedColor
]

{ #category : 'resize/collapse' }
SystemWindow >> reframePanesAdjoining: growingPane along: side to: aDisplayBox [
	| delta newRect minDim theMin horiz |
	growingPane ifNil: [^ self].  "As from click outside"
	newRect := aDisplayBox.
	horiz := #(left right) includes: side.
	theMin := horiz ifTrue: [40] ifFalse: [20].

	"First check that this won't make any pane smaller than theMin screen dots"
	minDim := (((paneMorphs select: [:pane | pane bounds bordersOn: growingPane bounds along: side])
		collect: [:pane | pane bounds adjustTo: newRect along: side]) copyWith: aDisplayBox)
			inject: 999 into:
				[:was :rect | was min: (horiz ifTrue: [rect width] ifFalse: [rect height])].
	"If so, amend newRect as required"
	minDim > theMin ifFalse:
		[delta := minDim - theMin.
		newRect := newRect withSide: side setTo:
				((newRect perform: side) > (growingPane bounds perform: side)
					ifTrue: [(newRect perform: side) + delta]
					ifFalse: [(newRect perform: side) - delta])].

	"Now adjust all adjoining panes for real"
	paneMorphs do:
		[:pane | (pane bounds bordersOn: growingPane bounds along: side) ifTrue:
			[pane bounds: (pane bounds adjustTo: newRect along: side)]].
	"And adjust the growing pane itself"
	growingPane bounds: newRect.

	"Finally force a recomposition of the whole window"
	self setPaneRectsFromBounds.
	self extent: self extent
]

{ #category : 'dropping/grabbing' }
SystemWindow >> rejectDropMorphEvent: evt [

	super rejectDropMorphEvent: evt.

	self formerOwner acceptDroppingMorph: self event: evt.
	self position: self formerPosition.
	
	self formerOwner: nil.
	self formerPosition: nil.
]

{ #category : 'accessing' }
SystemWindow >> relabel [
	| newLabel |
	newLabel := self morphicUIManager
		request: 'New title for this window' translated
		initialAnswer: labelString.
	newLabel isEmptyOrNil ifTrue: [^self].
	self setLabel: newLabel
]

{ #category : 'events' }
SystemWindow >> rememberKeyboardFocus: aMorph [
	"Record the current keyboard focus for the receiver."

	(aMorph isNil or: [(aMorph hasOwner: self) not]) ifFalse: [
		self setProperty: #rememberedFocus toValue: aMorph]
]

{ #category : 'events' }
SystemWindow >> rememberedKeyboardFocus [
	"Answer the remembered keyboard focus for the receiver."

	^self valueOfProperty: #rememberedFocus
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> removeBoxes [
	"Remove all label area boxes."

	closeBox ifNotNil: [closeBox delete. closeBox := nil].
	menuBox ifNotNil: [menuBox delete. menuBox := nil].
	expandBox ifNotNil: [expandBox delete. expandBox := nil].
	collapseBox ifNotNil: [collapseBox delete. collapseBox := nil]
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> removeCloseBox [
	"Remove the close box."

	closeBox ifNotNil: [closeBox delete. closeBox := nil]
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> removeCollapseBox [
	"Remove the collapse box."

	collapseBox ifNotNil: [collapseBox delete. collapseBox := nil]
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> removeEdgeGrips [
	"Remove the window edge grips."

	|edges|
	edges := self submorphsSatisfying: [:each | each isKindOf: WindowEdgeGripMorph].
	edges do: [:each | each delete]
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> removeExpandBox [
	"Remove the expand box."

	expandBox ifNotNil: [expandBox delete. expandBox := nil]
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> removeGrips [
	"Remove the edge and corner grips."

	self
		removeCornerGrips;
		removeEdgeGrips
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> removeLabelArea [
	"Remove the entire label area."

	self removeGrips.
	labelArea delete.
	label := nil.
	(self isCollapsed not or: [self isTaskbarPresent]) ifTrue: [
		self addGripsIfWanted]
]

{ #category : 'geometry' }
SystemWindow >> removeMenuBox [
	menuBox ifNotNil:
		[menuBox delete.
		menuBox := nil]
]

{ #category : 'submorphs - add/remove' }
SystemWindow >> replaceBoxes [
	"Rebuild the various boxes."

	labelArea removeAllMorphs.
	self setLabelWidgetAllowance.
	self theme configureWindowLabelAreaFor: self.
	self setFramesForLabelArea.
	self isActive ifFalse: [labelArea passivate]
]

{ #category : 'panes' }
SystemWindow >> replacePane: oldPane with: newPane [
	"Make newPane exactly occupy the position and extent of oldPane"

	| aLayoutFrame hadDep |
	hadDep := model dependents includes: oldPane.
	oldPane owner replaceSubmorph: oldPane by: newPane.
	newPane
		position: oldPane position;
		extent: oldPane extent.
	aLayoutFrame := oldPane layoutFrame.
	paneMorphs := paneMorphs collect:
		[:each |
		each == oldPane ifTrue: [newPane] ifFalse: [each]].
	aLayoutFrame ifNotNil: [newPane layoutFrame: aLayoutFrame].
	newPane color: Color transparent.
	hadDep ifTrue: [model removeDependent: oldPane. model addDependent: newPane].

	self changed
]

{ #category : 'accessing' }
SystemWindow >> resetCollapsedFrame [
	"Reset the collapsed frame."

	collapsedFrame := nil
]

{ #category : 'activation' }
SystemWindow >> restore [
	"Restore the receiver's normal size."

	self isMinimized
		ifTrue: [self collapseBoxHit]
		ifFalse: [self isMaximized ifTrue: [self expandBoxHit]]
]

{ #category : 'activation' }
SystemWindow >> restoreAndActivate [
	"Restore the window if minimised then activate."

	self isMinimized
		ifTrue: [self restore].
	self isActive
		ifFalse: [self activate]
]

{ #category : 'panes' }
SystemWindow >> restoreDefaultPaneColor [
	"Useful when changing from monochrome to color display"

	self setStripeColorsFrom: self paneColor
]

{ #category : 'drawing' }
SystemWindow >> scrollBarColor [
	^self paneColor
]

{ #category : 'menu actions' }
SystemWindow >> sendToBack [
    "Relies on #submorphs array being ordered top to bottom."
	| otherSystemWindows |
	otherSystemWindows := self world submorphs select: [ :morph | morph isSystemWindow and: morph ~~ self ].
	otherSystemWindows ifNotEmpty:
	[	otherSystemWindows first activate.
		self world addMorph: self behind: otherSystemWindows last.
	]
]

{ #category : 'resize/collapse' }
SystemWindow >> setBoundsWithFlex: newFrame [
	"Set bounds from newFrame with origin preserved from global coordinates"

	self isFlexed
		ifTrue: [super bounds: ((owner transform globalPointToLocal: newFrame topLeft)
										extent: newFrame extent)]
		ifFalse: [super bounds: newFrame]
]

{ #category : 'accessing' }
SystemWindow >> setExpandBoxBalloonText [
	"Set the expand box balloon help text as appropriate."

	expandBox ifNil: [^self].
	self unexpandedFrame
		ifNil: [expandBox setBalloonText: 'Expand to full screen' translated]
		ifNotNil: [expandBox setBalloonText: 'Contract to original size' translated]
]

{ #category : 'accessing' }
SystemWindow >> setFramesForLabelArea [
	"Set the layout for the label area."

	self theme configureWindowLabelAreaFrameFor: self
]

{ #category : 'accessing' }
SystemWindow >> setLabel: aString [
	| frame announcement |
	labelString := aString.
	announcement := (WindowLabelled new
						window: self;
						label: aString;
						yourself).
	self announce: announcement.
	self currentWorld announcer announce: announcement.
	label ifNil: [^ self].
	label contents: aString.
	self labelWidgetAllowance.  "Sets it if not already"
	self isCollapsed
		ifTrue: [self extent: (label width + labelWidgetAllowance) @ (self labelHeight + 2)]
		ifFalse: [
				"Add a tooltip on the label with the full content in case there is not enough space"
				(label width > (bounds width - labelWidgetAllowance))
					ifTrue: [ label setBalloonText: label contents ]
					ifFalse: [ label setBalloonText: nil ].

				label fitContents; setWidth: (label width min: bounds width - labelWidgetAllowance).
				label align: label bounds topCenter with: bounds topCenter + (0@borderWidth).
				collapsedFrame ifNotNil:
					[collapsedFrame := collapsedFrame withWidth: label width + labelWidgetAllowance]].
	frame := LayoutFrame new.
	frame leftFraction: 0.5;
		 topFraction: 0.5;
		 leftOffset: label width negated // 2;
		 topOffset: label height negated // 2.
	label layoutFrame: frame
]

{ #category : 'accessing' }
SystemWindow >> setLabelFont: aFont [

	label ifNil: [^ self].
	label font: aFont
]

{ #category : 'accessing' }
SystemWindow >> setLabelWidgetAllowance [
	"Set the extra space required, in general, apart from the label.
	Should make theme based (for centered titles), leave enough room
	for the moment."

	^labelWidgetAllowance :=  (self boxExtent x * 7)
	"allow for three on one side and centering plus a bit"
]

{ #category : 'geometry' }
SystemWindow >> setPaneRectsFromBounds [
	"Reset proportional specs from actual bounds, eg, after reframing panes"
	| layoutBounds |
	layoutBounds := self layoutBounds.
	paneMorphs do:[:m| | frame right top box bottom left |
		frame := m layoutFrame.
		box := m bounds.
		frame ifNotNil:[
			left := box left - layoutBounds left - (frame leftOffset ifNil:[0]).
			right := box right - layoutBounds left - (frame rightOffset ifNil:[0]).
			top := box top - layoutBounds top - (frame topOffset ifNil:[0]).
			bottom := box bottom - layoutBounds top - (frame bottomOffset ifNil:[0]).
			frame leftFraction: (left / layoutBounds width asFloat).
			frame rightFraction: (right / layoutBounds width asFloat).
			frame topFraction: (top / layoutBounds height asFloat).
			frame bottomFraction: (bottom / layoutBounds height asFloat).
		].
	]
]

{ #category : 'accessing' }
SystemWindow >> setStripeColorsFrom: paneColor [
	"Set the stripe color based on the given paneColor.
	Removed box color update for Pharo compatability."

	stripes ifNil: [^self].
	self fillStyle: self fillStyleToUse.
	self isActive
		ifTrue: [label ifNotNil: [label color: self activeLabelFillStyle].
				labelArea fillStyle: self activeTitleFillStyle]
		ifFalse: [label ifNotNil: [label color: self inactiveLabelFillStyle].
				labelArea fillStyle: self inactiveTitleFillStyle].
	self adoptPaneColor: self paneColor
]

{ #category : 'panes' }
SystemWindow >> setUpdatablePanesFrom: getSelectors [
	| aList possibles |
	"Set my updatablePanes inst var to the list of panes which are list panes with the given get-list selectors.  Order is important here!  Note that the method is robust in the face of panes not found, but a warning is printed in the transcript in each such case"
	aList := OrderedCollection new.
	possibles := OrderedCollection new.
	self
		allMorphsDo: [ :pane |
			(pane isKindOf: PluggableListMorph)
				ifTrue: [ possibles add: pane ] ].
	getSelectors
		do: [ :sel | possibles detect: [ :pane | pane getListSelector == sel ] ifFound: [ :aPane | aList add: aPane ] ].
	updatablePanes := aList asArray
]

{ #category : 'menu actions' }
SystemWindow >> setWindowColor [
	"Allow the user to select a new basic color for the window"

	(self morphicUIManager chooseColor: self paneColor)
		ifNotNil: [:nc | self setWindowColor: nc]
]

{ #category : 'accessing' }
SystemWindow >> setWindowColor: incomingColor [
	"Removed existing color check - looked useless!"

	| aColor |
	incomingColor ifNil: [^ self].  "it happens"
	aColor := incomingColor.
	self setProperty: #paneColor toValue: aColor.
	self setStripeColorsFrom: aColor.
	self theme fadedBackgroundWindows
		ifFalse: [self adoptPaneColor: aColor]. "reverse optimisation"
	self changed
]

{ #category : 'testing' }
SystemWindow >> shouldDropOnMouseUp [
	"Return true for consistency with fastdrag"
	^true
]

{ #category : 'controls' }
SystemWindow >> showAbout [

	self theme
		longMessageIn: self
		text: self aboutText
		title: self aboutTitle
]

{ #category : 'stepping' }
SystemWindow >> stepAt: millisecondClockValue [
	"If the receiver is not collapsed, step it, after first stepping the model."

	(isCollapsed not or: [self wantsStepsWhenCollapsed]) ifTrue:
		[model ifNotNil: [model stepAt: millisecondClockValue in: self].
		super stepAt: millisecondClockValue "let player, if any, step"]

"Since this method ends up calling step, the model-stepping logic should not be duplicated there."
]

{ #category : 'stepping' }
SystemWindow >> stepTime [
	^ model
		ifNotNil: [model stepTimeIn: self]
		ifNil: [200] "milliseconds"
]

{ #category : 'icons' }
SystemWindow >> taskbarIcon [
	
	^ self iconFormSetNamed: self taskbarIconName
]

{ #category : 'icons' }
SystemWindow >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	self model ifNotNil: [
		self model taskbarIconName
			ifNotNil: [ :aName | ^aName ] ].

	^ super taskbarIconName
]

{ #category : 'move' }
SystemWindow >> taskbarMoveLeft [

	self moveTaskbarTaskToOffset: -1
]

{ #category : 'move' }
SystemWindow >> taskbarMoveRight [


	self moveTaskbarTaskToOffset: 1
]

{ #category : 'move' }
SystemWindow >> taskbarMoveTo [
	"Move the current task button to the prompted location"

	| taskBarSize taskLocation desiredLocation answer |
	taskBarSize := self worldTaskbar orderedTasks size.
	taskLocation := self worldTaskbar orderedTasks indexOf: self taskbarTask.

	answer := UIManager default request: 'Enter new task location: 1..' , taskBarSize printString initialAnswer: taskLocation asString title: 'Move Task'.

	answer ifNil: [ ^ self ].

	[
		desiredLocation := answer asNumber.
		((1 to: taskBarSize) includes: desiredLocation) ifFalse: [ self error: 'incorrect location' ] ] onErrorDo: [
			InformativeNotification signal: 'Location must be a number between 1 and ' , taskBarSize printString.
			^ self ].

	self moveTaskbarTaskToOffset: (taskLocation - desiredLocation) negated
]

{ #category : 'theme' }
SystemWindow >> theme [
	"Answer the ui theme that provides controls.
	Don't call super since that implementation may delegate here."

	(self valueOfProperty: #theme) ifNotNil: [:t | ^ t].
	^self class theme
]

{ #category : 'theme' }
SystemWindow >> themeChanged [
	"Update the window colour and control boxes."

	self labelArea delete.
	self removeGrips.
	self theme
		configureWindowBorderFor: self;
		configureWindowDropShadowFor: self.
	self paneColor: self defaultBackgroundColor.
	label ifNotNil: [ "don't if label area removed"
		self initializeLabelArea].
	self setStripeColorsFrom: self paneColor.
	(self isCollapsed not or: [self isTaskbarPresent]) ifTrue: [
		self addGripsIfWanted].

	self isEmbedded ifTrue: [ self borderWidth: 0 ].

	super themeChanged
]

{ #category : 'open/close' }
SystemWindow >> toggleClosable [
	"Reinstate the close box. Go via theme to maintain box order."

	mustNotClose
	ifTrue: [ self makeClosable ]
	ifFalse: [ self makeUnclosable ]
]

{ #category : 'top window' }
SystemWindow >> topWindow [
	^ self class topWindow
]

{ #category : 'resize/collapse' }
SystemWindow >> unexpandedFrame [
	"Return the frame size of an unexpanded window"

	^ self valueOfProperty: #unexpandedFrame
]

{ #category : 'resize/collapse' }
SystemWindow >> unexpandedFrame: aRectangle [
	"Set the frame size of an unexpanded window"

	^ self setProperty: #unexpandedFrame toValue: aRectangle
]

{ #category : 'panes' }
SystemWindow >> updatablePanes [
	"Answer the list of panes, in order, which should be sent the #verifyContents message"
	^ updatablePanes ifNil: [updatablePanes := #()]
]

{ #category : 'accessing' }
SystemWindow >> update: aSymbol [
	aSymbol = #relabel
		ifTrue: [^ model ifNotNil: [ self setLabel: model labelString ] ].
	aSymbol = #close
		ifTrue: [self delete]
]

{ #category : 'panes' }
SystemWindow >> updatePaneColors [
	"Useful when changing from monochrome to color display"

	self setStripeColorsFrom: self paneColorToUse
]

{ #category : 'top window' }
SystemWindow >> updatePanesFromSubmorphs [
	"Having removed some submorphs, make sure this is reflected in my paneMorphs."
	paneMorphs := paneMorphs select: [ :pane | submorphs includes: pane ]
]

{ #category : 'accessing' }
SystemWindow >> useHideForClose [

	^ self class useHideForClose
]

{ #category : 'resize/collapse' }
SystemWindow >> wantsExpandBox [
	"Answer whether I'd like an expand box"

	^ true
]

{ #category : 'testing' }
SystemWindow >> wantsGrips [
	"Answer whether the window wants edge and corner grips."

	^self isResizeable
]

{ #category : 'events' }
SystemWindow >> wantsHalo [
	^ false
]

{ #category : 'resize/collapse' }
SystemWindow >> wantsKeyboardFocus [

	^ false
]

{ #category : 'testing' }
SystemWindow >> wantsRoundedCorners [
	"Answer whether rounded corners are wanted."

	^(self theme windowPreferredCornerStyleFor: self) == #rounded
]

{ #category : 'stepping' }
SystemWindow >> wantsSteps [
	"Return true if the model wants its view to be stepped.  For an open system window, we give the model to offer an opinion"

	isCollapsed ifTrue: [ ^ false ].

	model ifNil: [ ^ false ].

	(model respondsTo: #wantsSteps) ifFalse: [ ^ false ].

	^ model wantsSteps
]

{ #category : 'stepping' }
SystemWindow >> wantsStepsWhenCollapsed [
	"Default is not to bother updating collapsed windows"

	^ false
]

{ #category : 'testing' }
SystemWindow >> wantsToBeCachedByHand [
	"Return true if the receiver wants to be cached by the hand when it is dragged around."

	self isTranslucentButNotTransparent
		ifTrue: [ ^ false ].
	self clipSubmorphs
		ifTrue: [ ^ true ].
	self bounds = self fullBounds
		ifTrue: [ ^ true ].
	self
		submorphsDo: [ :m |
			(self bounds containsRect: m fullBounds)
				ifFalse: [
					m wantsToBeCachedByHand
						ifFalse: [ ^ false ] ] ].
	^ true
]

{ #category : 'events' }
SystemWindow >> wantsToBeDroppedInto: aMorph [
	"Return true if it's okay to drop the receiver into aMorph"
	^ aMorph isWorldMorph or: [self embeddable]
]

{ #category : 'menu actions' }
SystemWindow >> wantsYellowButtonMenu [
	"Answer true if the receiver wants a yellow button menu"
	^ false
]

{ #category : 'accessing' }
SystemWindow >> widthOfFullLabelText [
	^StandardFonts windowTitleFont widthOfString: labelString
]

{ #category : 'testing' }
SystemWindow >> window [
	"Answer the receiver's window."

	^self
]
