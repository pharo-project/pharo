Extension { #name : 'Morph' }

{ #category : '*Morphic-Halos' }
Morph >> addAddHandMenuItemsForHalo: aMenu hand: aHandMorph [
	"The former charter of this method was to add halo menu items that pertained specifically to the hand.  Over time this charter has withered, and most morphs reimplement this method simply to add their morph-specific menu items.  So in the latest round, all other implementors in the standard image have been removed.  However, this is left here as a hook for the benefit of existing code in client uses."
]

{ #category : '*Morphic-Halos' }
Morph >> addBorderStyleMenuItems: aMenu hand: aHandMorph [
	"Probably one could offer border-style items even if it's not a borderedMorph, so this remains a loose end for the moment"
]

{ #category : '*Morphic-Halos' }
Morph >> addCellLayoutMenuItems: aMenu hand: aHand [
	"Cell (e.g., child) related items"
	| menu sub |
	menu := self morphicUIManager newMenuIn: self for: self.
	menu addUpdating: #hasDisableTableLayoutString selector: #changeDisableTableLayout.
	menu addLine.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#rigid #shrinkWrap #spaceFill)
		do: [ :sym |
			sub
				addUpdating: #hResizingString:
				target: self
				selector: #hResizing:
				argumentList: (Array with: sym) ].
	menu add: 'horizontal resizing' subMenu: sub.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#rigid #shrinkWrap #spaceFill)
		do: [ :sym |
			sub
				addUpdating: #vResizingString:
				target: self
				selector: #vResizing:
				argumentList: (Array with: sym) ].
	menu add: 'vertical resizing' subMenu: sub.
	aMenu ifNotNil: [ aMenu add: 'child layout' subMenu: menu ].
	^ menu
]

{ #category : '*Morphic-Halos' }
Morph >> addCustomHaloMenuItems: aMenu hand: aHandMorph [
	"Add morph-specific items to the given menu which was invoked by the given hand from the halo.  To get started, we defer to the counterpart method used with the option-menu, but in time we can have separate menu choices for halo-menus and for option-menus"

	self addCustomMenuItems: aMenu hand: aHandMorph
]

{ #category : '*Morphic-Halos' }
Morph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
	"Add morph-specific items to the given menu which was invoked by the given hand.  This method provides is invoked both from the halo-menu and from the control-menu regimes."
]

{ #category : '*Morphic-Halos' }
Morph >> addDebuggingItemsTo: aMenu hand: aHandMorph [
	aMenu add: 'debug...' subMenu: (self buildDebugMenu: aHandMorph).
	aMenu lastItem icon: (self iconNamed: #smallDebug)
]

{ #category : '*Morphic-Halos' }
Morph >> addDropShadowMenuItems: aMenu hand: aHand [
	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	menu
		addUpdating: #hasDropShadowString
		selector: #toggleDropShadow.
	menu addLine.
	menu add: 'shadow color...' target: self selector: #changeShadowColor.
	menu add: 'shadow offset...' target: self selector: #setShadowOffset:.
	aMenu add: 'drop shadow' subMenu: menu
]

{ #category : '*Morphic-Halos' }
Morph >> addEmbeddingMenuItemsTo: aMenu hand: aHandMorph [
	"Construct a menu offerring embed targets for the receiver.  If the incoming menu is is not degenerate, add the constructed menu as a submenu; in any case, answer the embed-target menu"

	| menu potentialEmbeddingTargets |
	potentialEmbeddingTargets := self potentialEmbeddingTargets.
	potentialEmbeddingTargets size > 1
		ifFalse: [ ^ self ].
	menu := self morphicUIManager newMenuIn: self for: self.
	potentialEmbeddingTargets
		reverseDo: [ :m |
			menu
				add: m class name asString
				target: m
				selector: #addMorphFrontFromWorldPosition:
				argument: self topRendererOrSelf.
			menu lastItem icon: (m iconOrThumbnailOfSize: 16).
			self owner == m
				ifTrue: [ menu lastItem emphasis: 1 ] ].
	aMenu add: 'embed into' subMenu: menu.
	^ menu
]

{ #category : '*Morphic-Halos' }
Morph >> addExportMenuItems: aMenu hand: aHandMorph [
	"Add export items to the menu"

	aMenu
		ifNotNil: [ | aSubMenu |
			aSubMenu := self morphicUIManager newMenuIn: self for: self.
			aSubMenu add: 'BMP file' selector: #exportAsBMP.
			aSubMenu add: 'GIF file' selector: #exportAsGIF.
			aSubMenu add: 'JPEG file' selector: #exportAsJPEG.
			aSubMenu add: 'PNG file' selector: #exportAsPNG.
			aMenu
				add: 'export...'
				icon: (self iconNamed: #smallExport)
				subMenu: aSubMenu.
			aMenu lastItem
				icon: (self iconNamed: #smallExport) ]
]

{ #category : '*Morphic-Halos' }
Morph >> addFillStyleMenuItems: aMenu hand: aHand [
	"Add the items for changing the current fill style of the Morph"
	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	self fillStyle addFillStyleMenuItems: menu hand: aHand from: self.
	menu addLine.
	menu add: 'solid fill' selector: #useSolidFill.
	menu add: 'gradient fill' selector: #useGradientFill.
	menu add: 'bitmap fill' selector: #useBitmapFill.
	menu add: 'default fill' selector: #useDefaultFill.
	aMenu add: 'fill style' subMenu: menu
]

{ #category : '*Morphic-Halos' }
Morph >> addHalo [
	"Invoke a halo programatically (e.g., not from a meta gesture)"
	^self addHalo: nil
]

{ #category : '*Morphic-Halos' }
Morph >> addHalo: evt [
	| halo |

	self halosEnabled
		ifFalse: [ ^ self ].

	halo := (Smalltalk globals at: self haloClass ifAbsent: [ HaloMorph ]) new.
	halo bounds: (halo worldBoundsForMorph: self).
	halo popUpFor: self event: evt.
	^ halo
]

{ #category : '*Morphic-Halos' }
Morph >> addHalo: evt from: formerHaloOwner [
	"Transfer a halo from the former halo owner to the receiver"
	^self addHalo: evt
]

{ #category : '*Morphic-Halos' }
Morph >> addHaloActionsTo: aMenu [
	"Add items to aMenu representing actions requestable via halo"

	| subMenu |
	subMenu := self morphicUIManager newMenuIn: self for: self.
	subMenu
	buildTitle: [ :menuTitle | menuTitle
		bigTitle: self externalName
	];
	add: 'delete' selector: #dismissViaHalo.
	subMenu balloonTextForLastItem: 'Delete this object -- warning -- can be destructive!' translated.

	self maybeAddCollapseItemTo: subMenu.
	subMenu add: 'grab' selector: #openInHand.
	subMenu balloonTextForLastItem: 'Pick this object up -- warning, since this removes it from its container, it can have adverse effects.' translated.

	subMenu addLine.

	subMenu add: 'resize'  selector: #resizeFromMenu.
	subMenu balloonTextForLastItem: 'Change the size of this object' translated.

	subMenu add: 'duplicate'  selector: #maybeDuplicateMorph.
	subMenu balloonTextForLastItem: 'Hand me a copy of this object'.

	subMenu addLine.

	subMenu add: 'set color'  target: self renderedMorph selector: #changeColor.
	subMenu balloonTextForLastItem: 'Change the color of this object'.

	subMenu addLine.

	subMenu add: 'inspect' target: self selector: #inspect.
	subMenu balloonTextForLastItem: 'Open an Inspector on this object'.

	aMenu add: 'halo actions...' subMenu: subMenu
]

{ #category : '*Morphic-Halos' }
Morph >> addHandlesTo: aHaloMorph box: box [
	"Add halo handles to the halo.  Apply the halo filter if appropriate"


	aHaloMorph haloBox: box.
	HaloMorph  currentHaloSpecifications do:
		[:aSpec | | wantsIt aSelector |
			aSelector := aSpec addHandleSelector.
			(wantsIt := self wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph)
				ifTrue: [(#(addDupHandle:) includes: aSelector)
					ifTrue: [wantsIt := self preferredDuplicationHandleSelector = aSelector]].
			wantsIt
				ifTrue: [aHaloMorph perform: aSelector with: aSpec]].
	aHaloMorph innerTarget addOptionalHandlesTo: aHaloMorph box: box
]

{ #category : '*Morphic-Halos' }
Morph >> addLayoutMenuItems: topMenu hand: aHand [
	| aMenu |
	aMenu := self morphicUIManager newMenuIn: self for: self.
	aMenu addUpdating: #hasNoLayoutString selector: #changeNoLayout.
	aMenu addUpdating: #hasProportionalLayoutString selector: #changeProportionalLayout.
	aMenu addUpdating: #hasTableLayoutString selector: #changeTableLayout.
	aMenu addLine.
	aMenu add: 'change layout inset...' selector: #changeLayoutInset:.
	aMenu addLine.
	self addCellLayoutMenuItems: aMenu hand: aHand.
	self addTableLayoutMenuItems: aMenu hand: aHand.
	topMenu ifNotNil: [ topMenu add: 'layout' subMenu: aMenu ].
	^aMenu
]

{ #category : '*Morphic-Halos' }
Morph >> addMiscExtrasTo: aMenu [
	"Add a submenu of miscellaneous extra items to the menu."

	| subMenu |
	self isWorldMorph
		ifTrue: [ ^ self ].
	subMenu := self morphicUIManager newMenuIn: self for: self.
	self renderedMorph isSystemWindow
		ifFalse: [ subMenu add: 'put in a window' selector: #embedInWindow ].
	subMenu add: 'adhere to edge...' selector: #adhereToEdge.
	aMenu add: 'extras...' subMenu: subMenu
]

{ #category : '*Morphic-Halos' }
Morph >> addModelYellowButtonItemsTo: aCustomMenu event: evt [
	"Give my models a chance to add their context-menu items to
	aCustomMenu."
	self model
		ifNotNil: [:mod |
			mod
				addModelYellowButtonMenuItemsTo: aCustomMenu
				forMorph: self
				hand: evt hand]
]

{ #category : '*Morphic-Halos' }
Morph >> addMyYellowButtonMenuItemsToSubmorphMenus [
	"Answer true if I have items to add to the context menus of my submorphs"

	^true
]

{ #category : '*Morphic-Halos' }
Morph >> addNestedYellowButtonItemsTo: aMenu event: evt [
	"Add items to aMenu starting with me and proceeding down
	through my submorph chain,
	letting any submorphs that include the event position
	contribute their items to the bottom of the menu, separated by
	a line."

	| underMouse submenu |
	self addYellowButtonMenuItemsTo: aMenu event: evt.
	underMouse := self submorphThat: [ :each | each containsPoint: evt position ] ifNone: [ ^ self ].
	(underMouse addMyYellowButtonMenuItemsToSubmorphMenus and: [ underMouse hasYellowButtonMenu ])
		ifFalse: [ ^ self ].
	aMenu addLine.
	submenu := self morphicUIManager newMenuIn: underMouse for: underMouse.
	underMouse addNestedYellowButtonItemsTo: submenu event: evt.
	aMenu add: underMouse externalName icon: (underMouse iconOrThumbnailOfSize: 16) subMenu: submenu
]

{ #category : '*Morphic-Halos' }
Morph >> addOptionalHandlesTo: aHalo box: box [
]

{ #category : '*Morphic-Halos' }
Morph >> addStandardHaloMenuItemsTo: aMenu hand: aHandMorph [
	"Add standard halo items to the menu"

	aMenu add: 'send to back' selector: #goBehind.
	aMenu add: 'bring to front' selector: #comeToFront.
	self addEmbeddingMenuItemsTo: aMenu hand: aHandMorph.
	aMenu addLine.
	self addFillStyleMenuItems: aMenu hand: aHandMorph.
	self addBorderStyleMenuItems: aMenu hand: aHandMorph.
	self addDropShadowMenuItems: aMenu hand: aHandMorph.
	self addLayoutMenuItems: aMenu hand: aHandMorph.
	self addHaloActionsTo: aMenu.
	owner isTextMorph
		ifTrue: [ self addTextAnchorMenuItems: aMenu hand: aHandMorph ].
	aMenu addLine.
	self addToggleItemsToHaloMenu: aMenu.
	aMenu addLine.
	self addExportMenuItems: aMenu hand: aHandMorph.
	self addMiscExtrasTo: aMenu.
	self addDebuggingItemsTo: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu defaultTarget: aHandMorph
]

{ #category : '*Morphic-Halos' }
Morph >> addTableLayoutMenuItems: aMenu hand: aHand [
	| menu sub |
	menu := self morphicUIManager newMenuIn: self for: self.
	menu addUpdating: #hasReverseCellsString selector: #changeReverseCells.
	menu addUpdating: #hasClipLayoutCellsString selector: #changeClipLayoutCells.
	menu addUpdating: #hasRubberBandCellsString selector: #changeRubberBandCells.
	menu addLine.
	menu add: 'change cell inset...' selector: #changeCellInset:.
	menu add: 'change min cell size...' selector: #changeMinCellSize:.
	menu add: 'change max cell size...' selector: #changeMaxCellSize:.
	menu addLine.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#leftToRight #rightToLeft #topToBottom #bottomToTop)
		do: [ :sym |
			sub
				addUpdating: #listDirectionString:
				target: self
				selector: #changeListDirection:
				argumentList: (Array with: sym) ].
	menu add: 'list direction' subMenu: sub.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#none #leftToRight #rightToLeft #topToBottom #bottomToTop)
		do: [ :sym |
			sub
				addUpdating: #wrapDirectionString:
				target: self
				selector: #wrapDirection:
				argumentList: (Array with: sym) ].
	menu add: 'wrap direction' subMenu: sub.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#center #topLeft #topRight #bottomLeft #bottomRight #topCenter #leftCenter #rightCenter #bottomCenter)
		do: [ :sym |
			sub
				addUpdating: #cellPositioningString:
				target: self
				selector: #cellPositioning:
				argumentList: (Array with: sym) ].
	menu add: 'cell positioning' subMenu: sub.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#topLeft #bottomRight #center #justified)
		do: [ :sym |
			sub
				addUpdating: #listCenteringString:
				target: self
				selector: #listCentering:
				argumentList: (Array with: sym) ].
	menu add: 'list centering' subMenu: sub.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#topLeft #bottomRight #center #justified)
		do: [ :sym |
			sub
				addUpdating: #wrapCenteringString:
				target: self
				selector: #wrapCentering:
				argumentList: (Array with: sym) ].
	menu add: 'wrap centering' subMenu: sub.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#none #equal)
		do: [ :sym |
			sub
				addUpdating: #listSpacingString:
				target: self
				selector: #listSpacing:
				argumentList: (Array with: sym) ].
	menu add: 'list spacing' subMenu: sub.
	sub := self morphicUIManager newMenuIn: self for: self.
	#(#none #localRect #localSquare #globalRect #globalSquare)
		do: [ :sym |
			sub
				addUpdating: #cellSpacingString:
				target: self
				selector: #cellSpacing:
				argumentList: (Array with: sym) ].
	menu add: 'cell spacing' subMenu: sub.
	aMenu ifNotNil: [ aMenu add: 'table layout' subMenu: menu ].
	^ menu
]

{ #category : '*Morphic-Halos' }
Morph >> addTextAnchorMenuItems: topMenu hand: aHand [
	| aMenu |
	aMenu := self morphicUIManager newMenuIn: self for: self.
	aMenu addUpdating: #hasInlineAnchorString selector: #changeInlineAnchor.
	aMenu addUpdating: #hasParagraphAnchorString selector: #changeParagraphAnchor.
	aMenu addUpdating: #hasDocumentAnchorString selector: #changeDocumentAnchor.
	topMenu ifNotNil: [ topMenu add: 'text anchor' subMenu: aMenu ].
	^aMenu
]

{ #category : '*Morphic-Halos' }
Morph >> addTitleForHaloMenu: aMenu [
	aMenu buildTitle: [ :menuTitle | menuTitle
		bigTitle: self externalName;
		icon: (self iconOrThumbnailOfSize: 18);
		cellInset: 3
	]
]

{ #category : '*Morphic-Halos' }
Morph >> addToggleItemsToHaloMenu: aMenu [
	"Add standard true/false-checkbox items to the memu"

	#(#(#resistsRemovalString #toggleResistsRemoval 'whether I should be reistant to easy deletion via the pink X handle' true) #(#stickinessString #toggleStickiness 'whether I should be resistant to a drag done by mousing down on me' true) #(#lockedString #lockUnlockMorph 'when "locked", I am inert to all user interactions' true) #(#hasClipSubmorphsString #changeClipSubmorphs 'whether the parts of objects within me that are outside my bounds should be masked.' false) #(#hasDirectionHandlesString #changeDirectionHandles 'whether direction handles are shown with the halo' false) #(#hasDragAndDropEnabledString #changeDragAndDrop 'whether I am open to having objects dropped into me' false))
		do: [ :each |
			aMenu addUpdating: each first selector: each second.
			aMenu balloonTextForLastItem: each third translated ].
	self couldHaveRoundedCorners
		ifFalse: [ ^ self ].
	aMenu addUpdating: #roundedCornersString selector: #toggleCornerRounding.
	aMenu balloonTextForLastItem: 'whether my corners should be rounded' translated
]

{ #category : '*Morphic-Halos' }
Morph >> addWorldHandlesTo: aHaloMorph box: box [
	aHaloMorph haloBox: box.
	HaloMorph haloSpecificationsForWorld
		do: [:aSpec | aHaloMorph perform: aSpec addHandleSelector with: aSpec].
	aHaloMorph innerTarget addOptionalHandlesTo: aHaloMorph box: box
]

{ #category : '*Morphic-Halos' }
Morph >> addYellowButtonMenuItemsTo: aMenu event: evt [
	"Populate aMenu with appropriate menu items for a
	yellow-button (context menu) click."
	aMenu defaultTarget: self.
	self defaultYellowButtonMenuEnabled ifFalse: [^ self].
	"aMenu addStayUpItem."
	self addModelYellowButtonItemsTo: aMenu event: evt.
	self cmdGesturesEnabled ifTrue: [
			aMenu addLine.
			aMenu add: 'inspect' selector: #inspect].
	aMenu addLine.
	self isWorldMorph ifFalse: [aMenu add: 'delete' selector: #delete].
	self world selectedObject == self
		ifTrue: [aMenu add: 'halo off' selector: #removeHalo]
		ifFalse: [aMenu add: 'halo on' selector: #addHalo].
	(self isWorldMorph
			or: [self wantsToBeTopmost])
		ifFalse: [
			aMenu addLine.
			aMenu add: 'send to back' selector: #goBehind.
			aMenu add: 'bring to front' selector: #comeToFront.
			self addEmbeddingMenuItemsTo: aMenu hand: evt hand].
	self isWorldMorph ifFalse: [
			self isFullOnScreen ifFalse: [aMenu add: 'move onscreen' selector: #goHome]].
			self addLayoutMenuItems: aMenu hand: evt hand.
			(owner isNotNil
					and: [owner isTextMorph])
				ifTrue: [self addTextAnchorMenuItems: aMenu hand: evt hand].
	self isWorldMorph ifFalse: [
			aMenu addLine.
			self addToggleItemsToHaloMenu: aMenu].
	aMenu addLine.
	self isWorldMorph
		ifFalse: [aMenu add: 'copy to paste buffer' selector: #copyToPasteBuffer:].
	(self allStringsAfter: nil) isEmpty ifFalse: [aMenu add: 'copy text' selector: #clipText].
	self addExportMenuItems: aMenu hand: evt hand.
	aMenu addLine.
	aMenu add: 'adhere to edge...' selector: #adhereToEdge.
	self addCustomMenuItems: aMenu hand: evt hand
]

{ #category : '*Morphic-Halos' }
Morph >> adhereToEdge [
	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	#(top right bottom left - center - topLeft topRight bottomRight bottomLeft - none)
		do: [:each |
			each == #-
				ifTrue: [menu addLine]
				ifFalse: [menu add: each asString selector: #setToAdhereToEdge: argument: each]].
	menu popUpEvent: self currentEvent in: self world
]

{ #category : '*Morphic-Halos' }
Morph >> adhereToEdge: edgeSymbol [
	| edgeMessage |
	(owner isNil or: [owner isHandMorph]) ifTrue: [^self].

	"these are implemented in Morph, no need to use perform:"
	edgeSymbol == #bottomLeft ifTrue: [ ^ self bottomLeft: owner bottomLeft ].
	edgeSymbol == #bottomRight ifTrue: [ ^ self bottomRight: owner bottomRight ].
	edgeSymbol == #adjustedCenter ifTrue: [ ^ self adjustedCenter: owner adjustedCenter ].

	(owner class canUnderstand:  edgeSymbol) ifFalse:  [^self].
	(self class canUnderstand: ( edgeMessage := (edgeSymbol , ':') asSymbol ))
		 ifFalse:  [^self].

	self perform: edgeMessage
		withArguments: (Array with: (owner perform: edgeSymbol))
]

{ #category : '*Morphic-Halos' }
Morph >> adjustedCenter [
	"Provides a hook for objects to provide a reference point other than the receiver's center,for the purpose of centering a submorph under special circumstances, such as BalloonMorph"

	^ self center
]

{ #category : '*Morphic-Halos' }
Morph >> adjustedCenter: c [
	"Set the receiver's position based on the #adjustedCenter protocol for adhereToEdge.  By default this simply sets the receiver's center.   Though there are (at its inception anyway) no other implementors of this method, it is required in use with the #adhereToEdge when the centering of a submorph is to be with reference to a rectangle  other than the receiver's center."

	self center: c
]

{ #category : '*Morphic-Halos' }
Morph >> balloonHelpTextForHandle: aHandle [
	"Answer a string providing balloon help for the
	given halo handle"
	(aHandle eventHandler mouseSelectorsInclude: #doRecolor:with:) ifTrue: [^ 'Change color'].
	(aHandle eventHandler mouseSelectorsInclude: #mouseDownInDimissHandle:with:) ifTrue: [^ 'Remove from screen' translated].
	#(#(#addFullHandles 'More halo handles')  #(#chooseEmphasisOrAlignment 'Emphasis & alignment') #(#chooseFont 'Change font') #(#chooseNewGraphicFromHalo 'Choose a new graphic') #(#chooseStyle 'Change style') #(#doDebug:with: 'Debug') #(#doDirection:with: 'Choose forward direction') #(#doDup:with: 'Duplicate')  #(#doMenu:with: 'Menu') #(#doGrab:with: 'Pick up')  #(#mouseDownInCollapseHandle:with: 'Collapse') #(#mouseDownOnHelpHandle: 'Help')  #(#prepareToTrackCenterOfRotation:with: 'Move object or set center of rotation') #(#startDrag:with: 'Move') #(#startGrow:with: 'Change size') #(#startRot:with: 'Rotate') #(#startScale:with: 'Change scale')#(#trackCenterOfRotation:with: 'Set center of rotation') )
		do: [:pair | (aHandle eventHandler mouseSelectorsInclude: pair first)
				ifTrue: [^ pair last]].
	^ 'unknown halo handle' translated
]

{ #category : '*Morphic-Halos' }
Morph >> buildDebugMenu: aHand [
	"Answer a debugging menu for the receiver.
	 The hand argument is seemingly historical and plays no role presently"

	| aMenu |
	aMenu := self morphicUIManager newMenuIn: self for: self.
	(self hasProperty: #errorOnDraw)
		ifTrue: [ aMenu add: 'start drawing again' selector: #resumeAfterDrawError ].
	(self hasProperty: #drawError)
		ifTrue: [ aMenu add: 'debug drawing error' selector: #debugDrawError.
			aMenu addLine ].
	(self hasProperty: #errorOnStep)
		ifTrue: [ aMenu add: 'start stepping again' selector: #resumeAfterStepError.
			aMenu addLine ].
	aMenu add: 'inspect morph' selector: #inspectInMorphic:.
	aMenu lastItem
		icon: (self iconNamed: #smallInspectIt).
	aMenu add: 'inspect owner chain' selector: #inspectOwnerChain.
	aMenu lastItem
		icon: (self iconNamed: #smallInspectIt).
	aMenu addLine.
	aMenu
		add: 'browse morph class'
		target: self
		selector: #browseHierarchy.
	^ aMenu
]

{ #category : '*Morphic-Halos' }
Morph >> buildHandleMenu: aHand [
	"Build the morph menu for the given morph's halo's menu handle. This menu has two sections. The first section contains commands that are interpreted by the hand; the second contains commands provided by the target morph. This method allows the morph to decide which items should be included in the hand's section of the menu."

	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	menu addLine.
	self addStandardHaloMenuItemsTo: menu hand: aHand.
	menu defaultTarget: aHand.
	self addAddHandMenuItemsForHalo: menu  hand: aHand.
	menu defaultTarget: self.
	self addCustomHaloMenuItems: menu hand: aHand.
	menu defaultTarget: aHand.
	^ menu
]

{ #category : '*Morphic-Halos' }
Morph >> buildMetaMenu: evt [
	"Build the morph menu. This menu has two sections. The first section contains commands that are handled by the hand; the second contains commands handled by the argument morph."

	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	menu add: 'grab' selector: #grabMorph:.
	menu add: 'copy to paste buffer' selector: #copyToPasteBuffer:.
	self maybeAddCollapseItemTo: menu.
	menu add: 'delete' selector: #dismissMorph:.
	menu addLine.
	menu add: 'copy text' selector: #clipText.
	menu addLine.
	menu add: 'go behind' selector: #goBehind.
	menu add: 'add halo' selector: #addHalo:.
	menu add: 'duplicate' selector: #maybeDuplicateMorph:.
	self addEmbeddingMenuItemsTo: menu hand: evt hand.
	menu add: 'resize' selector: #resizeMorph:.
	"Give the argument control over what should be done about fill styles"
	self addFillStyleMenuItems: menu hand: evt hand.
	self addDropShadowMenuItems: menu hand: evt hand.
	self addLayoutMenuItems: menu hand: evt hand.
	menu
		addUpdating: #hasClipSubmorphsString
		target: self
		selector: #changeClipSubmorphs
		argumentList: #().
	menu addLine.
	menu
		add: 'inspect'
		selector: #inspectAt:event:
		argument: evt position.
	menu add: 'explore' selector: #inspect.
	menu lastItem
		icon: (self iconNamed: #smallInspectIt).
	menu addLine.
	menu add: 'show actions' selector: #showActions.
	menu addLine.
	self addDebuggingItemsTo: menu hand: evt hand.
	self addCustomMenuItems: menu hand: evt hand.
	^ menu
]

{ #category : '*Morphic-Halos' }
Morph >> buildYellowButtonMenu: aHand [
	"build the morph menu for the yellow button"
	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	self addNestedYellowButtonItemsTo: menu event: self activeHand lastEvent.
	^ menu
]

{ #category : '*Morphic-Halos' }
Morph >> cellPositioningString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self cellPositioning
]

{ #category : '*Morphic-Halos' }
Morph >> cellSpacingString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self cellSpacing
]

{ #category : '*Morphic-Halos' }
Morph >> changeCellInset: evt [
	| handle |
	handle := HandleMorph new forEachPointDo:[:newPoint |
		self cellInset: (newPoint - evt cursorPoint) asIntegerPoint // 5].
	evt hand attachMorph: handle.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
Morph >> changeClipLayoutCells [
	self invalidRect: self fullBounds.
	self clipLayoutCells: self clipLayoutCells not.
	self invalidRect: self fullBounds
]

{ #category : '*Morphic-Halos' }
Morph >> changeColor [

	| dialog |
	dialog := ColorSelectorDialogWindow new
		title: 'Choose color';
		selectedColor: self color.
	dialog openModal.
	dialog cancelled
		ifFalse: [self fillStyle: dialog selectedColor]
]

{ #category : '*Morphic-Halos' }
Morph >> changeColorTarget: anObject selector: aSymbol originalColor: aColor hand: aHand [
	"Put up a color picker for changing some kind of color."

	(self morphicUIManager chooseColor: aColor)
		ifNotNil: [:nc | anObject perform: aSymbol with: nc]
]

{ #category : '*Morphic-Halos' }
Morph >> changeDirectionHandles [
	^self wantsDirectionHandles: self wantsDirectionHandles not
]

{ #category : '*Morphic-Halos' }
Morph >> changeDisableTableLayout [
	self disableTableLayout: self disableTableLayout not.
	self layoutChanged
]

{ #category : '*Morphic-Halos' }
Morph >> changeDocumentAnchor [
	"Change the anchor from/to document anchoring"

	| newType |
	newType := self textAnchorType == #document
		ifTrue: [#paragraph]
		ifFalse: [ #document].
	owner isTextMorph
		ifTrue:
			[owner
				anchorMorph: self
				at: self position
				type: newType]
]

{ #category : '*Morphic-Halos' }
Morph >> changeDragAndDrop [
	^self enableDragNDrop: self dragNDropEnabled not
]

{ #category : '*Morphic-Halos' }
Morph >> changeInlineAnchor [
	"Change the anchor from/to line anchoring"

	| newType |
	newType := self textAnchorType == #inline
				ifTrue: [#paragraph]
				ifFalse: [#inline].
	owner isTextMorph
		ifTrue:
			[owner
				anchorMorph: self
				at: self position
				type: newType]
]

{ #category : '*Morphic-Halos' }
Morph >> changeLayoutInset: evt [
	| handle |
	handle := HandleMorph new forEachPointDo:[:newPoint |
		self layoutInset: (newPoint - evt cursorPoint) asIntegerPoint // 5].
	evt hand attachMorph: handle.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
Morph >> changeListDirection: aSymbol [
	| listDir wrapDir |
	self listDirection: aSymbol.
	(self wrapDirection == #none) ifTrue:[^self].
	"otherwise automatically keep a valid table layout"
	listDir := self listDirection.
	wrapDir := self wrapDirection.
	(listDir == #leftToRight or:[listDir == #rightToLeft]) ifTrue:[
		wrapDir == #leftToRight ifTrue:[^self wrapDirection: #topToBottom].
		wrapDir == #rightToLeft ifTrue:[^self wrapDirection: #bottomToTop].
	] ifFalse:[
		wrapDir == #topToBottom ifTrue:[^self wrapDirection: #leftToRight].
		wrapDir == #bottomToTop ifTrue:[^self wrapDirection: #rightToLeft].
	]
]

{ #category : '*Morphic-Halos' }
Morph >> changeMaxCellSize: evt [
	| handle |
	handle := HandleMorph new forEachPointDo:[:newPoint |
		self maxCellSize: (newPoint - evt cursorPoint) asIntegerPoint].
	evt hand attachMorph: handle.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
Morph >> changeMinCellSize: evt [
	| handle |
	handle := HandleMorph new forEachPointDo:[:newPoint |
		self minCellSize: (newPoint - evt cursorPoint) asIntegerPoint].
	evt hand attachMorph: handle.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
Morph >> changeNoLayout [
	self layoutPolicy ifNil:[^self]. "already no layout"
	self layoutPolicy: nil.
	self layoutChanged
]

{ #category : '*Morphic-Halos' }
Morph >> changeParagraphAnchor [
	"Change the anchor from/to paragraph anchoring"

	| newType |
	newType := self textAnchorType == #paragraph
		ifTrue: [#document]
		ifFalse: [#paragraph].
	owner isTextMorph
		ifTrue:
			[owner
				anchorMorph: self
				at: self position
				type: newType]
]

{ #category : '*Morphic-Halos' }
Morph >> changeReverseCells [
	self reverseTableCells: self reverseTableCells not
]

{ #category : '*Morphic-Halos' }
Morph >> changeRubberBandCells [
	self rubberBandCells: self rubberBandCells not
]

{ #category : '*Morphic-Halos' }
Morph >> changeShadowColor [
	"Change the shadow color of the receiver -- triggered, e.g. from a menu"

	(self morphicUIManager chooseColor: self shadowColor)
		ifNotNil: [:nc | self shadowColor: nc]
]

{ #category : '*Morphic-Halos' }
Morph >> comeToFrontAndAddHalo [
	self comeToFront.
	self addHalo
]

{ #category : '*Morphic-Halos' }
Morph >> copyToPasteBuffer: evt [
	^evt hand copyToPasteBuffer: self
]

{ #category : '*Morphic-Halos' }
Morph >> defersHaloOnClickTo: aSubMorph [
	"If a cmd-click on aSubMorph would make it a preferred recipient of the halo, answer true"
	"May want to add a way (via a property) for morphs to assert true here -- this would let certain kinds of morphs that are unusually reluctant to take the halo on initial click"

	^ false
]

{ #category : '*Morphic-Halos' }
Morph >> dismissMorph: evt [
	self dismissMorph
]

{ #category : '*Morphic-Halos' }
Morph >> dismissViaHalo [
	"The user has clicked in the delete halo-handle.  This provides a hook in case some concomitant action should be taken, or if the particular morph is not one which should be put in the trash can, for example."

	self setProperty: #lastPosition toValue: self positionInWorld.
	self dismissMorph
]

{ #category : '*Morphic-Halos' }
Morph >> duplicateMorph: evt [
	"Make and return a duplicate of the receiver's argument"
	| dup |
	dup := self duplicate.
	evt hand grabMorph: dup from: owner. "duplicate was ownerless so use #grabMorph:from: here"
	^dup
]

{ #category : '*Morphic-Halos' }
Morph >> editBalloonHelpContent: aString [
	| reply |
	reply := self morphicUIManager
		multiLineRequest: 'Edit the balloon help text for ' translated, self externalName
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])
		answerHeight: 200.
	reply ifNil: [^ self].  "User cancelled out of the dialog"
	(reply isEmpty or: [reply asString = self noHelpString])
		ifTrue: [self setBalloonText: nil]
		ifFalse: [self setBalloonText: reply]
]

{ #category : '*Morphic-Halos' }
Morph >> grabMorph: evt [

	evt hand grabMorph: self
]

{ #category : '*Morphic-Halos' }
Morph >> hResizingString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self hResizing
]

{ #category : '*Morphic-Halos' }
Morph >> halo [
	^ self primaryHand halo
		ifNotNil: [ :h | h target == self ifTrue: [ h ] ]
]

{ #category : '*Morphic-Halos' }
Morph >> haloClass [
	"Answer the name of the desired kind of HaloMorph to launch on behalf of the receiver"

	^ #HaloMorph
]

{ #category : '*Morphic-Halos' }
Morph >> hasClipLayoutCellsString [
	^ (self clipLayoutCells) -> 'clip to cell size' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasDirectionHandlesString [
	^ (self wantsDirectionHandles) ->  'direction handles' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasDisableTableLayoutString [
	^ (self disableTableLayout) -> 'disable layout in tables' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasDocumentAnchorString [
	^ (self textAnchorType == #document) ->  'Document' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasDragAndDropEnabledString [
	"Answer a string to characterize the drag & drop status of the
	receiver"
	^ (self dragNDropEnabled) -> 'accept drops' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasDropShadowString [
	^ (self hasDropShadow) ->  'show shadow' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasHalo [
	^self hasProperty: #hasHalo
]

{ #category : '*Morphic-Halos' }
Morph >> hasHalo: aBool [
	aBool
		ifTrue:[self setProperty: #hasHalo toValue: true]
		ifFalse:[self removeProperty: #hasHalo]
]

{ #category : '*Morphic-Halos' }
Morph >> hasInlineAnchorString [
	^ (self textAnchorType == #inline)-> 'Inline' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasNoLayoutString [
	^ (self layoutPolicy isNil) -> 'no layout' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasParagraphAnchorString [
	^ (self textAnchorType == #paragraph) -> 'Paragraph' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasProportionalLayoutString [
	| layout |
	^ ((layout := self layoutPolicy) isNotNil
			and: [layout isProportionalLayout]) -> 'proportional layout' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasReverseCellsString [
	^ (self reverseTableCells) -> 'reverse table cells' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasRubberBandCellsString [
	^ (self rubberBandCells) -> 'rubber band cells' translated
]

{ #category : '*Morphic-Halos' }
Morph >> hasTableLayoutString [
	| layout |
	^ ((layout := self layoutPolicy) isNotNil
			and: [layout isTableLayout]) ->   'table layout' translated
]

{ #category : '*Morphic-Halos' }
Morph >> inspectAt: aPoint event: evt [
	|  morphs target |
	morphs := self morphsAt: aPoint.
	(morphs includes: self) ifFalse:[morphs := morphs copyWith: self].
	target := self morphicUIManager
				chooseFrom: (morphs
						collect: [:t |t class name asString])
				values: morphs
				title: ('inspect whom? (deepest at top)' translated).
	target ifNil:[^self].
	target inspectInMorphic: evt
]

{ #category : '*Morphic-Halos' }
Morph >> inspectInMorphic: evt [
	(Smalltalk tools toolNamed: #inspector) inspect: self
]

{ #category : '*Morphic-Halos' }
Morph >> inspectOwnerChain [
	self ownerChain inspectWithLabel: 'Owner chain for ', self printString
]

{ #category : '*Morphic-Halos' }
Morph >> layoutMenuPropertyString: aSymbol from: currentSetting [
	| wording |
	wording := String
				streamContents: [:stream |
					| index |
					index := 1.
					aSymbol keysAndValuesDo: [:idx :ch | ch isUppercase
								ifTrue: [stream nextPutAll: (aSymbol copyFrom: index to: idx - 1) asLowercase.
										stream nextPutAll: ' '.
										index := idx]].
					index < aSymbol size
						ifTrue: [stream nextPutAll: (aSymbol copyFrom: index to: aSymbol size) asLowercase]].
	^ (aSymbol == currentSetting) -> wording translated
]

{ #category : '*Morphic-Halos' }
Morph >> listCenteringString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self listCentering
]

{ #category : '*Morphic-Halos' }
Morph >> listDirectionString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self listDirection
]

{ #category : '*Morphic-Halos' }
Morph >> listSpacingString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self listSpacing
]

{ #category : '*Morphic-Halos' }
Morph >> lockUnlockMorph [
	"If the receiver is locked, unlock it; if unlocked, lock it"

	self isLocked ifTrue: [self unlock] ifFalse: [self lock]
]

{ #category : '*Morphic-Halos' }
Morph >> lockedString [
	"Answer the string to be shown in a menu to represent the 'locked' status"
	^ (self isLocked) -> 'be locked' translated
]

{ #category : '*Morphic-Halos' }
Morph >> maybeAddCollapseItemTo: aMenu [
	"If appropriate, add a collapse item to the given menu"

	| anOwner |
	(anOwner := self topRendererOrSelf owner) ifNotNil:
			[anOwner isWorldMorph ifTrue:
				[aMenu add: 'collapse' target: self selector: #collapse]]
]

{ #category : '*Morphic-Halos' }
Morph >> maybeDuplicateMorph [
	"Maybe duplicate the morph"

	^self duplicate openInHand
]

{ #category : '*Morphic-Halos' }
Morph >> maybeDuplicateMorph: evt [
	^self duplicateMorph: evt
]

{ #category : '*Morphic-Halos' }
Morph >> model [
	^ nil
]

{ #category : '*Morphic-Halos' }
Morph >> mouseDownOnHelpHandle: anEvent [
	"The mouse went down in the show-balloon handle"

	| str |
	anEvent shiftPressed ifTrue: [^ self editBalloonHelpText].
	str := self balloonText.
	str ifNil: [str := self noHelpString].
	self showBalloon: str hand: anEvent hand
]

{ #category : '*Morphic-Halos' }
Morph >> okayToAddDismissHandle [
	"Answer whether a halo on the receiver should offer a dismiss handle.  This provides a hook for making it harder to disassemble some strucures even momentarily"

	^self resistsRemoval not
]

{ #category : '*Morphic-Halos' }
Morph >> potentialEmbeddingTargets [
	"Return the potential targets for embedding the receiver"

	| oneUp topRend |
	(oneUp := (topRend := self topRendererOrSelf) owner) ifNil: [ ^ #() ].
	^ (oneUp morphsAt: topRend referencePosition behind: topRend unlocked: true) reject: [ :m | m isFlexMorph ]
]

{ #category : '*Morphic-Halos' }
Morph >> potentialTargets [
	"Return the potential targets for the receiver.
	This is derived from Morph>>potentialEmbeddingTargets."
	owner ifNil:[^#()].
	^owner morphsAt: self referencePosition behind: self unlocked: true not
]

{ #category : '*Morphic-Halos' }
Morph >> potentialTargetsAt: aPoint [
	"Return the potential targets for the receiver.
	This is derived from Morph>>potentialEmbeddingTargets."
	| realOwner |
	realOwner := self topRendererOrSelf
	owner
		ifNil: [^ #()].
	^ realOwner
		morphsAt: aPoint
]

{ #category : '*Morphic-Halos' }
Morph >> preferredDuplicationHandleSelector [
	"Answer the selector, either #addMakeSiblingHandle: or addDupHandle:, to be offered as the default in a halo open on me"

	^ #addDupHandle:
]

{ #category : '*Morphic-Halos' }
Morph >> removeHalo [
	"remove the surrounding halo (if any)"
	self primaryHand removeHaloAround: self
]

{ #category : '*Morphic-Halos' }
Morph >> resistsRemovalString [
	"Answer the string to be shown in a menu to represent the
	'resistsRemoval' status"
	^ (self resistsRemoval) -> 'resist being deleted' translated
]

{ #category : '*Morphic-Halos' }
Morph >> resizeFromMenu [
	"Commence an interaction that will resize the receiver"

	self resizeMorph: self activeHand lastEvent
]

{ #category : '*Morphic-Halos' }
Morph >> resizeMorph: evt [
	| handle |
	handle := HandleMorph new forEachPointDo: [ :newPoint | self extent: newPoint - self bounds topLeft ].
	evt hand attachMorph: handle.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
Morph >> setShadowOffset: evt [
	| handle |
	handle := HandleMorph new forEachPointDo:
		[:newPoint | self shadowPoint: newPoint].
	evt hand attachMorph: handle.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
Morph >> showActions [
	"Put up a message list browser of all the code that this morph
	would run for mouseUp, mouseDown, mouseMove, mouseEnter,
	mouseLeave, and mouseLinger."

	| list cls selector adder |
	list := SortedCollection new.
	adder := [:mrClass :mrSel | list
				add: (RGMethodDefinition realClass: mrClass selector: mrSel)].
	"the eventHandler"
	self eventHandler
		ifNotNil: [list := self eventHandler methodRefList.
			(self eventHandler handlesMouseDown: nil)
				ifFalse: [adder value: HandMorph value: #grabMorph:]].
	"If not those, then non-default raw events"
	#(#keyStroke: #mouseDown: #mouseEnter: #mouseLeave: #mouseMove: #mouseUp: #doButtonAction )
		do: [:sel |
			cls := self class whichClassIncludesSelector: sel.
			cls
				ifNotNil: ["want more than default behavior"
					cls == Morph
						ifFalse: [adder value: cls value: sel]]].
	"The mechanism on a Button"
	(self respondsTo: #actionSelector)
		ifTrue: ["A button"
			selector := self actionSelector.
			cls := self target class whichClassIncludesSelector: selector.
			cls
				ifNotNil: ["want more than default behavior"
					cls == Morph
						ifFalse: [adder value: cls value: selector]]].
	(Smalltalk tools toolNamed: #messageList) browse: list title:  'Actions of ' , self printString from: self class
]

{ #category : '*Morphic-Halos' }
Morph >> snapToEdgeIfAppropriate [
	| edgeSymbol oldBounds aWorld |
	(edgeSymbol := self valueOfProperty: #edgeToAdhereTo) ifNotNil:
		[oldBounds := bounds.
		self adhereToEdge: edgeSymbol.
		bounds ~= oldBounds ifTrue: [(aWorld := self world) ifNotNil: [aWorld viewBox ifNotNil:
			[aWorld displayWorld]]]]
]

{ #category : '*Morphic-Halos' }
Morph >> stickinessString [
	"Answer the string to be shown in a menu to represent the stickiness status"

	^ (self isSticky) ->  'resist being picked up' translated
]

{ #category : '*Morphic-Halos' }
Morph >> targetWith: evt [
	"Some other morph become target of the receiver"
	|  newTarget |
	newTarget := self morphicUIManager
				chooseFrom: (self potentialTargets
						collect: [:t | t class name asString])
				values: self potentialTargets
				title: (self externalName, ' targets...' translated).
	newTarget ifNil:[^self].
	self target: newTarget
]

{ #category : '*Morphic-Halos' }
Morph >> transferHalo: event from: formerHaloOwner [
	"Progressively transfer the halo to the next likely recipient"
	| localEvt w target |

	self flag: #workAround. "For halo's distinction between 'target' and 'innerTarget' we need to bypass any renderers."
	(formerHaloOwner == self and:[self isRenderer and:[self wantsHaloFromClick not]]) ifTrue:[
		event shiftPressed ifTrue:[
			target := owner.
			localEvt := event transformedBy: (self transformedFrom: owner).
		] ifFalse:[
			target := self renderedMorph.
			localEvt := event transformedBy: (target transformedFrom: self).
		].
		^target transferHalo: localEvt from: target].

"	formerHaloOwner == self ifTrue:[^ self removeHalo]."

	"Never transfer halo to top-most world"
	(self isWorldMorph and:[owner isNil]) ifFalse:[
		(self wantsHaloFromClick and:[formerHaloOwner ~~ self])
			ifTrue:[^self addHalo: event from: formerHaloOwner]].

	event shiftPressed ifTrue:[
		"Pass it outwards"
		owner ifNotNil:[^owner transferHalo: event from: formerHaloOwner].
		"We're at the top level; throw the event back in to find recipient"
		formerHaloOwner removeHalo.
		^self processEvent: event copy resetHandlerFields.
	].
	self submorphsDo:[:m|
		localEvt := event transformedBy: (m transformedFrom: self).
		(m fullContainsPoint: localEvt position)
			ifTrue:[^m transferHalo: event from: formerHaloOwner].
	].
	"We're at the bottom most level; throw the event back up to the root to find recipient"
	formerHaloOwner removeHalo.


	(w := self world) ifNil: [ ^self ].
	localEvt := event transformedBy: (self transformedFrom: w) inverseTransformation.
	^ w processEvent: localEvt resetHandlerFields
]

{ #category : '*Morphic-Halos' }
Morph >> vResizingString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self vResizing
]

{ #category : '*Morphic-Halos' }
Morph >> wantsDirectionHandles [
	^self valueOfProperty: #wantsDirectionHandles ifAbsent:[false]
]

{ #category : '*Morphic-Halos' }
Morph >> wantsDirectionHandles: aBool [
	self setProperty: #wantsDirectionHandles toValue: aBool
]

{ #category : '*Morphic-Halos' }
Morph >> wantsHaloFor: aSubMorph [
	^ false
]

{ #category : '*Morphic-Halos' }
Morph >> wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph [
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addDismissHandle:) includes: aSelector) ifTrue:
		[^ self resistsRemoval not].

	(#( addDragHandle: ) includes: aSelector) ifTrue:
		[^ self okayToBrownDragEasily].

	(#(addGrowHandle: addScaleHandle:) includes: aSelector) ifTrue:
		[^ self okayToResizeEasily].

	(#( addRotateHandle: ) includes: aSelector) ifTrue:
		[^ self okayToRotateEasily].

	(#(addRecolorHandle:) includes: aSelector) ifTrue:
		[^ self renderedMorph wantsRecolorHandle].

	^ true
]

{ #category : '*Morphic-Halos' }
Morph >> wantsRecolorHandle [
	"Answer whether the receiver would like a recoloring halo handle to be put up.  Since this handle also presently affords access to the property-sheet, it is presently always allowed, even though SketchMorphs don't like regular recoloring"

	^ true
]

{ #category : '*Morphic-Halos' }
Morph >> wrapCenteringString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self wrapCentering
]

{ #category : '*Morphic-Halos' }
Morph >> wrapDirectionString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self wrapDirection
]
