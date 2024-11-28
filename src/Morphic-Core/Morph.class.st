"
A Morph (from the Greek ""shape"" or ""form"") is an interactive graphical object. General information on the Morphic system can be found at http://minnow.cc.gatech.edu/squeak/30. 

Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, set its #visible property to false using the #visible: method. 

The World (screen) coordinate system is used for most coordinates, but can be changed if there is a TransformMorph somewhere in the owner chain. 

My instance variables have accessor methods (e.g., #bounds, #bounds:). Most users should use the accessor methods instead of using the instance variables directly.

Structure:
instance var 	Type 			Description 
bounds 			Rectangle 		A Rectangle indicating my position and a size that will enclose 									me. 
owner 			Morph		 	My parent Morph, or nil for the top-level Morph, which is a
 				or nil			world, typically a PasteUpMorph.
submorphs 		Array 			My child Morphs. 
fullBounds 		Rectangle 		A Rectangle minimally enclosing me and my submorphs. 
color 			Color 			My primary color. Subclasses can use this in different ways. 
extension 		MorphExtension Allows extra properties to be stored without adding a
				or nil  				storage burden to all morphs. 

By default, Morphs do not position their submorphs. Morphs may position their submorphs directly or use a LayoutPolicy to automatically control their submorph positioning.

Although Morph has some support for BorderStyle, most users should use BorderedMorph if they want borders.
"
Class {
	#name : 'Morph',
	#superclass : 'Object',
	#instVars : [
		'bounds',
		'owner',
		'submorphs',
		'fullBounds',
		'color',
		'extension'
	],
	#classVars : [
		'CmdGesturesEnabled',
		'CycleHalosBothDirections',
		'DefaultYellowButtonMenuEnabled',
		'EmptyArray',
		'EventDispatcher',
		'HalosEnabled',
		'ShortcutsHandler'
	],
	#classInstVars : [
		'announcer'
	],
	#category : 'Morphic-Core-Kernel',
	#package : 'Morphic-Core',
	#tag : 'Kernel'
}

{ #category : 'accessing' }
Morph class >> announcer [

	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : 'settings' }
Morph class >> cmdGesturesEnabled [
	^ CmdGesturesEnabled ifNil: [CmdGesturesEnabled := true]
]

{ #category : 'settings' }
Morph class >> cmdGesturesEnabled: aBoolean [
	CmdGesturesEnabled := aBoolean
]

{ #category : 'settings' }
Morph class >> cycleHalosBothDirections [

	^ CycleHalosBothDirections ifNil: [ CycleHalosBothDirections := false ]
]

{ #category : 'settings' }
Morph class >> cycleHalosBothDirections: aBoolean [

	CycleHalosBothDirections := aBoolean
]

{ #category : 'accessing' }
Morph class >> defaultEventDispatcher [
	^ EventDispatcher ifNil: [ EventDispatcher := MorphicEventDispatcher new ]
]

{ #category : 'settings' }
Morph class >> defaultYellowButtonMenuEnabled [
	^ DefaultYellowButtonMenuEnabled ifNil: [DefaultYellowButtonMenuEnabled := false]
]

{ #category : 'settings' }
Morph class >> defaultYellowButtonMenuEnabled: aBoolean [
	DefaultYellowButtonMenuEnabled := aBoolean
]

{ #category : 'settings' }
Morph class >> halosEnabled [

	^ HalosEnabled ifNil: [ HalosEnabled := true ]
]

{ #category : 'settings' }
Morph class >> halosEnabled: aBoolean [

	HalosEnabled := aBoolean
]

{ #category : 'class initialization' }
Morph class >> initialize [
	"Morph initialize"

	"this empty array object is shared by all morphs with no submorphs:"
	EmptyArray := Array new
]

{ #category : 'settings' }
Morph class >> morphNavigationShortcutsOn: aBuilder [
	"Basic, general navigation shortcut among morphs. #MorphNoCtrl will not work for TextMorphs."

	<keymap>
	(aBuilder shortcut: #navigateFocusForwardCtrl)
		category: #MorphFocusCtrlNavigation
		default: Character tab ctrl asKeyCombination
		do: [ :target :morph :event | morph navigateFocusForward ].
	(aBuilder shortcut: #navigateFocusBackwardCtrl)
		category: #MorphFocusCtrlNavigation
		default: Character tab shift ctrl asKeyCombination
		do: [ :target :morph :event | morph navigateFocusBackward ].
	(aBuilder shortcut: #navigateFocusForward)
		category: #MorphFocusNavigation
		default: Character tab asKeyCombination
		do: [ :target :morph :event | morph navigateFocusForward ].
	(aBuilder shortcut: #navigateFocusBackward)
		category: #MorphFocusNavigation
		default: Character tab shift asKeyCombination
		do: [ :target :morph :event | morph navigateFocusBackward ]
]

{ #category : 'misc' }
Morph class >> morphsUnknownToTheirOwners [
	"Return a list of all morphs (other than HandMorphs) whose owners do not contain them in their submorph lists"

	"Morph morphsUnknownToTheirOwners"

	| problemMorphs |
	problemMorphs := OrderedCollection new.
	self allSubInstances
		do: [ :m |
			| itsOwner |
			(m isHandMorph not and: [ (itsOwner := m owner) isNotNil and: [ (itsOwner submorphs includes: m) not ] ])
				ifTrue: [ problemMorphs add: m ] ].
	^ problemMorphs
]

{ #category : 'instance creation' }
Morph class >> newBounds: bounds [

	^ self new privateBounds: bounds
]

{ #category : 'instance creation' }
Morph class >> newBounds: bounds color: color [

	^ (self new privateBounds: bounds) privateColor: color
]

{ #category : 'instance creation' }
Morph class >> newSticky [

	^ self new beSticky
]

{ #category : 'shortcuts' }
Morph class >> shortcutsHandler [

	^ ShortcutsHandler
]

{ #category : 'shortcuts' }
Morph class >> shortcutsHandler: anObject [

	ShortcutsHandler := anObject
]

{ #category : 'dropping/grabbing' }
Morph >> aboutToBeGrabbedBy: aHand [
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand."

	self formerOwner: owner.
	self formerPosition: self position.
	^self "Grab me"
]

{ #category : 'layout' }
Morph >> acceptDroppingMorph: aMorph event: evt [
	"This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:Event: message. This default implementation just adds the given morph to the receiver."
	| layout |
	layout := self layoutPolicy.
	layout ifNil:[^self addMorph: aMorph].
	self privateAddMorph: aMorph
		atIndex: (layout indexForInserting: aMorph at: evt position in: self)
]

{ #category : 'events - accessing' }
Morph >> actionMap [
	"Answer an action map"

	^self updateableActionMap
]

{ #category : 'structure' }
Morph >> activeHand [
	| world |
   world := self world ifNil: [^ nil ].
   ^ world activeHand
]

{ #category : 'menus' }
Morph >> addAddHandMenuItemsForHalo: aMenu hand: aHandMorph [
	"The former charter of this method was to add halo menu items that pertained specifically to the hand.  Over time this charter has withered, and most morphs reimplement this method simply to add their morph-specific menu items.  So in the latest round, all other implementors in the standard image have been removed.  However, this is left here as a hook for the benefit of existing code in client uses."
]

{ #category : 'events - alarms' }
Morph >> addAlarm: aSelector after: delayTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: #() after: delayTime
]

{ #category : 'events - alarms' }
Morph >> addAlarm: aSelector at: scheduledTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: #() at: scheduledTime
]

{ #category : 'events - alarms' }
Morph >> addAlarm: aSelector with: arg1 after: delayTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: (Array with: arg1) after: delayTime
]

{ #category : 'events - alarms' }
Morph >> addAlarm: aSelector with: arg1 at: scheduledTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: (Array with: arg1) at: scheduledTime
]

{ #category : 'events - alarms' }
Morph >> addAlarm: aSelector with: arg1 with: arg2 after: delayTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: (Array with: arg1 with: arg2) after: delayTime
]

{ #category : 'events - alarms' }
Morph >> addAlarm: aSelector with: arg1 with: arg2 at: scheduledTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: (Array with: arg1 with: arg2) at: scheduledTime
]

{ #category : 'events - alarms' }
Morph >> addAlarm: aSelector withArguments: args after: delayTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: args at: Time millisecondClockValue + delayTime
]

{ #category : 'events - alarms' }
Morph >> addAlarm: aSelector withArguments: args at: scheduledTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	| scheduler |
	scheduler := self alarmScheduler.
	scheduler ifNotNil:[scheduler addAlarm: aSelector withArguments: args for: self at: scheduledTime]
]

{ #category : 'submorphs - add/remove' }
Morph >> addAllMorphs: aCollection [
	^self privateAddAllMorphs: aCollection atIndex: submorphs size
]

{ #category : 'submorphs - add/remove' }
Morph >> addAllMorphs: aCollection after: anotherMorph [
	^self privateAddAllMorphs: aCollection
			atIndex: (submorphs indexOf: anotherMorph ifAbsent: [submorphs size])
]

{ #category : 'menu' }
Morph >> addBorderStyleMenuItems: aMenu hand: aHandMorph [
	"Probably one could offer border-style items even if it's not a borderedMorph, so this remains a loose end for the moment"
]

{ #category : 'layout - menu' }
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

{ #category : 'menus' }
Morph >> addCustomHaloMenuItems: aMenu hand: aHandMorph [
	"Add morph-specific items to the given menu which was invoked by the given hand from the halo.  To get started, we defer to the counterpart method used with the option-menu, but in time we can have separate menu choices for halo-menus and for option-menus"

	self addCustomMenuItems: aMenu hand: aHandMorph
]

{ #category : 'menus' }
Morph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
	"Add morph-specific items to the given menu which was invoked by the given hand.  This method provides is invoked both from the halo-menu and from the control-menu regimes."
]

{ #category : 'debug and other' }
Morph >> addDebuggingItemsTo: aMenu hand: aHandMorph [
	aMenu add: 'debug...' subMenu: (self buildDebugMenu: aHandMorph).
	aMenu lastItem icon: (self iconNamed: #smallDebug)
]

{ #category : 'updating' }
Morph >> addDependent: anObject [

	self announcer
		when: MorphChanged , MorphChangedWithArguments
		send: #handleUpdate:
		to: anObject.

	^ anObject
]

{ #category : 'drop shadows' }
Morph >> addDropShadow [

	self hasDropShadow ifTrue:[^self].
	self changed.
	self hasDropShadow: true.
	self shadowOffset: 3@3.
	self layoutChanged.
	self changed
]

{ #category : 'drop shadows' }
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

{ #category : 'meta-actions' }
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

{ #category : 'menus' }
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

{ #category : 'menus' }
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

{ #category : 'rotate scale and flex' }
Morph >> addFlexShell [

	"Wrap a rotating and scaling shell around this morph."

	| oldHalo flexMorph myWorld anIndex |
	myWorld := self world.
	oldHalo := self halo.
	anIndex := self owner submorphIndexOf: self.
	self owner
		addMorph: (flexMorph := self newTransformationMorph asFlexOf: self)
		asElementNumber: anIndex.
	self transferStateToRenderer: flexMorph.
	oldHalo ifNotNil: [ oldHalo setTarget: flexMorph ].
	myWorld ifNotNil: [ flexMorph startSteppingSubmorphs ].

	^ flexMorph
]

{ #category : 'halos and balloon help' }
Morph >> addHalo [
	"Invoke a halo programatically (e.g., not from a meta gesture)"
	^self addHalo: nil
]

{ #category : 'halos and balloon help' }
Morph >> addHalo: evt from: formerHaloOwner [
	"Transfer a halo from the former halo owner to the receiver"
	^self addHalo: evt
]

{ #category : 'menus' }
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

{ #category : 'layout - menu' }
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

{ #category : 'menus' }
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

{ #category : 'menu' }
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

{ #category : 'submorphs - add/remove' }
Morph >> addMorph: aMorph [

	self addMorphFront: aMorph
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorph: newMorph after: aMorph [
	"Add the given morph as one of my submorphs, inserting it after anotherMorph"
	^self privateAddMorph: newMorph atIndex: (submorphs indexOf: aMorph)+1
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorph: aMorph asElementNumber: aNumber [
	"Add the given morph so that it becomes the aNumber'th element of my submorph list.  If aMorph is already one of my submorphs, reposition it"

	(submorphs includes: aMorph) ifTrue:
		[aMorph privateDelete].
	(aNumber <= submorphs size)
		ifTrue:
			[self addMorph: aMorph inFrontOf: (submorphs at: aNumber)]
		ifFalse:
			[self addMorphBack: aMorph]
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorph: newMorph behind: aMorph [
	"Add a morph to the list of submorphs behind the specified morph"
	^self privateAddMorph: newMorph atIndex: (submorphs indexOf: aMorph) + 1
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorph: aMorph fullFrame: aLayoutFrame [

	aMorph layoutFrame: aLayoutFrame asLayoutFrame.
	aMorph hResizing: #spaceFill; vResizing: #spaceFill.
	self addMorph: aMorph
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorph: newMorph inFrontOf: aMorph [
	"Add a morph to the list of submorphs in front of the specified morph"
	^self privateAddMorph: newMorph atIndex: ((submorphs indexOf: aMorph) max: 1)
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorphBack: aMorph [
	^self privateAddMorph: aMorph atIndex: submorphs size+1
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorphCentered: aMorph [

	aMorph position: bounds center - (aMorph extent // 2).
	self addMorphFront: aMorph
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorphFront: aMorph [
	^self privateAddMorph: aMorph atIndex: 1
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorphFront: aMorph fromWorldPosition: wp [

	self addMorphFront: aMorph.
	aMorph position: (self transformFromWorld globalPointToLocal: wp)
]

{ #category : 'submorphs - add/remove' }
Morph >> addMorphFrontFromWorldPosition: aMorph [
	^self addMorphFront: aMorph fromWorldPosition: aMorph positionInWorld
]

{ #category : 'wiw support' }
Morph >> addMorphInFrontOfLayer: aMorph [

	| targetLayer |

	targetLayer := aMorph morphicLayerNumber.
	submorphs do: [ :each | | layerHere |
		each == aMorph ifTrue: [^self].
		layerHere := each morphicLayerNumber.
		"the <= is the difference - it insures we go to the front of our layer"
		targetLayer <= layerHere ifTrue: [
			^self addMorph: aMorph inFrontOf: each
		].
	].
	self addMorphBack: aMorph
]

{ #category : 'wiw support' }
Morph >> addMorphInLayer: aMorph [

	submorphs do: [ :each |
		each == aMorph ifTrue: [^self].
		aMorph morphicLayerNumber < each morphicLayerNumber ifTrue: [
			^self addMorph: aMorph inFrontOf: each
		].
	].
	self addMorphBack: aMorph
]

{ #category : 'menu' }
Morph >> addMyYellowButtonMenuItemsToSubmorphMenus [
	"Answer true if I have items to add to the context menus of my submorphs"

	^true
]

{ #category : 'menu' }
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

{ #category : 'halos and balloon help' }
Morph >> addOptionalHandlesTo: aHalo box: box [
]

{ #category : 'menus' }
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

{ #category : 'layout - menu' }
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

{ #category : 'text-anchor' }
Morph >> addTextAnchorMenuItems: topMenu hand: aHand [
	| aMenu |
	aMenu := self morphicUIManager newMenuIn: self for: self.
	aMenu addUpdating: #hasInlineAnchorString selector: #changeInlineAnchor.
	aMenu addUpdating: #hasParagraphAnchorString selector: #changeParagraphAnchor.
	aMenu addUpdating: #hasDocumentAnchorString selector: #changeDocumentAnchor.
	topMenu ifNotNil: [ topMenu add: 'text anchor' subMenu: aMenu ].
	^aMenu
]

{ #category : 'menu' }
Morph >> addTitleForHaloMenu: aMenu [
	aMenu buildTitle: [ :menuTitle | menuTitle
		bigTitle: self externalName;
		icon: (self iconOrThumbnailOfSize: 18);
		cellInset: 3
	]
]

{ #category : 'menus' }
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

{ #category : 'utilities' }
Morph >> addTransparentSpacerOfSize: aPoint [
	self addMorphBack: (self transparentSpacerOfSize: aPoint)
]

{ #category : 'menu' }
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

{ #category : 'change reporting' }
Morph >> addedMorph: aMorph [
	"Notify the receiver that the given morph was just added."
]

{ #category : 'menus' }
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

{ #category : 'menus' }
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

{ #category : 'layout' }
Morph >> adjustLayoutBounds [
	"Adjust the receivers bounds depending on the resizing strategy imposed"

	| hFit vFit box myExtent extent |
	hFit := self hResizing.
	vFit := self vResizing.
	(hFit == #shrinkWrap or: [ vFit == #shrinkWrap ])
		ifFalse: [ ^ self ].	"not needed"
	box := self layoutBounds.
	myExtent := box extent.
	extent := self submorphBounds corner - box origin.
	hFit == #shrinkWrap
		ifTrue: [ myExtent := extent x @ myExtent y ].
	vFit == #shrinkWrap
		ifTrue: [ myExtent := myExtent x @ extent y ].	"Make sure we don't get smaller than minWidth/minHeight"
	myExtent x < self minWidth
		ifTrue: [ myExtent := (myExtent x max: self minWidth - self bounds width + self layoutBounds width) @ myExtent y ].
	myExtent y < self minHeight
		ifTrue: [ myExtent := myExtent x @ (myExtent y max: self minHeight - self bounds height + self layoutBounds height) ].
	self layoutBounds: (box origin extent: myExtent)
]

{ #category : 'menus' }
Morph >> adjustedCenter [
	"Provides a hook for objects to provide a reference point other than the receiver's center,for the purpose of centering a submorph under special circumstances, such as BalloonMorph"

	^ self center
]

{ #category : 'menus' }
Morph >> adjustedCenter: c [
	"Set the receiver's position based on the #adjustedCenter protocol for adhereToEdge.  By default this simply sets the receiver's center.   Though there are (at its inception anyway) no other implementors of this method, it is required in use with the #adhereToEdge when the centering of a submorph is to be with reference to a rectangle  other than the receiver's center."

	self center: c
]

{ #category : 'accessing' }
Morph >> adoptPaneColor: paneColor [
	self submorphsDo:[:m| m adoptPaneColor: paneColor]
]

{ #category : 'events - alarms' }
Morph >> alarmScheduler [
	"Return the scheduler being responsible for triggering alarms"
	^self world
]

{ #category : 'geometry' }
Morph >> align: aPoint1 with: aPoint2 [
	"Translate by aPoint2 - aPoint1."

	^ self position: self position + (aPoint2 - aPoint1)
]

{ #category : 'submorphs - accessing' }
Morph >> allMorphs [
	"Return a collection containing all morphs in this composite morph (including the receiver)."

	| all |
	all := (Array new: submorphs size) writeStream.
	self allMorphsDo: [:m | all nextPut: m].
	^ all contents
]

{ #category : 'submorphs - accessing' }
Morph >> allMorphsDo: aBlock [
	"Evaluate the given block for all morphs in this composite morph (including the receiver)."

	submorphs do: [:m | m allMorphsDo: aBlock].
	aBlock value: self
]

{ #category : 'submorphs - accessing' }
Morph >> allMorphsInto: aSet [
	"Return a set of all submorphs.  Don't forget the hidden ones. Consider only objects that are in memory (see allNonSubmorphMorphs)."

	submorphs do: [:m | m allMorphsInto: aSet].
	self allNonSubmorphMorphs do: [:m |
			(aSet includes: m) ifFalse: ["Stop infinite recursion"
				m allMorphsInto: aSet]].
	aSet add: self.
	^ aSet
]

{ #category : 'submorphs - accessing' }
Morph >> allNonSubmorphMorphs [
	"Return a collection containing all morphs in this morph which are not currently in the submorph containment hierarchy"

	^ OrderedCollection new
]

{ #category : 'structure' }
Morph >> allOwners [
	"Return the owners of the reciever"

	^ Array streamContents: [:strm | self allOwnersDo: [:m | strm nextPut: m]]
]

{ #category : 'structure' }
Morph >> allOwnersDo: aBlock [
	"Evaluate aBlock with all owners of the receiver"
	owner ifNotNil:[^owner withAllOwnersDo: aBlock]
]

{ #category : 'debug and other' }
Morph >> allStringsAfter: aSubmorph [
	"return an OrderedCollection of strings of text in my submorphs.  If aSubmorph is non-nil, begin with that container."

	| list ok |
	list := OrderedCollection new.
	ok := aSubmorph isNil.
	self allMorphsDo:
			[:sub | | string |
			ok ifFalse: [ok := sub == aSubmorph].	"and do this one too"
			ok
				ifTrue:
					[(string := sub userString) ifNotNil:
							[string isString ifTrue: [list add: string] ifFalse: [list addAll: string]]]].
	^list
]

{ #category : 'announcements' }
Morph >> announceDeleted [
	self doAnnounce: (MorphDeleted morph: self).
	self submorphs do: #announceDeleted
]

{ #category : 'announcements' }
Morph >> announceKeyboardFocusChange: gotFocus [
	| announcement |
	announcement := gotFocus
						ifTrue: [ MorphGotFocus morph: self ]
						ifFalse: [ MorphLostFocus morph: self ].
	self doAnnounce: announcement.
	Morph announcer announce: announcement
]

{ #category : 'announcements' }
Morph >> announceOpened [
	self doAnnounce: (MorphOpened morph: self).
	self submorphs do: #announceOpened
]

{ #category : 'announcements' }
Morph >> announcer [
	^self valueOfProperty: #announcer ifAbsentPut: [ Announcer new ]
]

{ #category : 'drawing' }
Morph >> areasRemainingToFill: aRectangle [
	"Pushed up from BorderedMorph, all cases tested for there are
	supported by basic Morph."

	"Morphs which achieve translucency by other means than fillStyle will have
	to reimplement this"

	"Fixed here to test the fillStyle rather than color for translucency.
	Since can have a translucent fillStyle while the (calculated) color is not."

	self fillStyle isTranslucent ifTrue: [ ^ Array with: aRectangle ].
	^ self wantsRoundedCorners
		ifTrue: [ (self borderWidth > 0 and: [ self borderColor isColor and: [ self borderColor isTranslucent ] ])
				ifTrue: [ aRectangle areasOutside: (self innerBounds intersect: self boundsWithinCorners) ]
				ifFalse: [ aRectangle areasOutside: self boundsWithinCorners ] ]
		ifFalse: [ (self borderWidth > 0 and: [ self borderColor isColor and: [ self borderColor isTranslucent ] ])
				ifTrue: [ aRectangle areasOutside: self innerBounds ]
				ifFalse: [ aRectangle areasOutside: self bounds ] ]
]

{ #category : 'stepping and presenter' }
Morph >> arrangeToStartStepping [
	"Arrange to start getting sent the 'step' message, but don't do that initial #step call that startStepping does"

	self arrangeToStartSteppingIn: self world
]

{ #category : 'stepping and presenter' }
Morph >> arrangeToStartSteppingIn: aWorld [
	"Start getting sent the 'step' message in aWorld.  Like startSteppingIn:, but without the initial one to get started'"

	aWorld ifNil: [ ^ self ].
	aWorld startStepping: self.
	self changed
]

{ #category : 'converting' }
Morph >> asDraggableMorph [
	^self
]

{ #category : 'converting' }
Morph >> asForm [

	^ self asFormOfSize: self extent
]

{ #category : 'converting' }
Morph >> asFormOfSize: extent [
	"Answer a new thumbnail for the receiver."
	| form transform rect |

	rect := self bounds scaledAndCenteredIn: (0@0 extent: extent).
	form := Form extent: rect extent depth: 32.
	transform := MatrixTransform2x3 withScale: form extent / self extent.
	form getCanvas
		transformBy: transform
		clippingTo: form boundingBox
		during: [ :canvas |
			canvas
				translateBy: self topLeft negated
				during: [ :translatedCanvas | self fullDrawOn: translatedCanvas ] ]
		smoothing: 2.
	^ form
]

{ #category : 'creation' }
Morph >> asMorph [
	^ self
]

{ #category : 'converting' }
Morph >> asReadOnlyMorph [

	^ self
]

{ #category : 'accessing - extension' }
Morph >> assureExtension [
	"creates an extension for the receiver if needed"
	^ extension ifNil: [extension := MorphExtension new]
]

{ #category : 'halos and balloon help' }
Morph >> balloonColor [
	^ self
		valueOfProperty: #balloonColor
		ifAbsent: [self defaultBalloonColor]
]

{ #category : 'halos and balloon help' }
Morph >> balloonColor: aColor [
	^ self
		setProperty: #balloonColor
		toValue: aColor
]

{ #category : 'halos and balloon help' }
Morph >> balloonFont [
	^ self
		valueOfProperty: #balloonFont
		ifAbsent: [self defaultBalloonFont]
]

{ #category : 'halos and balloon help' }
Morph >> balloonFont: aFont [
	^ self setProperty: #balloonFont toValue: aFont
]

{ #category : 'halos and balloon help' }
Morph >> balloonHelpAligner [
	"Answer the morph to which the receiver's balloon help should point"
	^ (self valueOfProperty: #balloonTarget) ifNil: [self]
]

{ #category : 'halos and balloon help' }
Morph >> balloonHelpDelayTime [
	"Return the number of milliseconds before a balloon help should be put up on the receiver. The balloon help will only be put up if the receiver responds to #wantsBalloon by returning true."
	^ self theme settings balloonHelpDelayTime
]

{ #category : 'settings' }
Morph >> balloonHelpEnabled [
	^ self theme settings balloonHelpEnabled
]

{ #category : 'halos and balloon help' }
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

{ #category : 'accessing' }
Morph >> balloonText [
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer"

	^ extension ifNotNil: [:ext | ext balloonText ifNotNil: [:text |
				text withNoLineLongerThan: self theme settings maxBalloonHelpLineLength]]
]

{ #category : 'theme' }
Morph >> basicTheme: aUITheme [
	"Set the current theme for the receiver without notification of change."

	self theme = aUITheme ifFalse: [
		self setProperty: #theme toValue: aUITheme]
]

{ #category : 'accessing' }
Morph >> beSticky [
	"make the receiver sticky"
	self assureExtension sticky: true
]

{ #category : 'accessing' }
Morph >> beTransparent [
	self color: Color transparent
]

{ #category : 'accessing' }
Morph >> beUnsticky [
	"If the receiver is marked as sticky, make it now be unsticky"
	extension ifNotNil: [extension sticky: false]
]

{ #category : 'user interface' }
Morph >> becomeModal [

	self world modalWindow: self
]

{ #category : 'base - widgets' }
Morph >> beginsWith: aString fromList: aMorph [
	| string |
	string := self userString ifNil: [(self submorphs collect: [:m | m userString]) detect: [:us | us isNotNil ] ifNone: ['']].

	^ string asString beginsWith: aString fromList: aMorph
]

{ #category : 'meta-actions' }
Morph >> blueButtonUp: anEvent [
	"Ignored. Theoretically we should never get here since control is transferred to the halo on #blueButtonDown: but subclasses may implement this differently."
]

{ #category : 'accessing' }
Morph >> borderColor [
	^self borderStyle color
]

{ #category : 'accessing' }
Morph >> borderStyle [

	^ extension
		ifNil: [BorderStyle default trackColorFrom: self]
		ifNotNil: [:ext | (ext borderStyle ifNil: [BorderStyle default]) trackColorFrom: self]
]

{ #category : 'accessing' }
Morph >> borderStyle: newStyle [
	newStyle = self borderStyle
		ifTrue: [ ^ self ].
	(self canDrawBorder: newStyle)
		ifFalse: [
			"Replace the suggested border with a simple one"
			^ self borderStyle: (BorderStyle width: newStyle width color: (newStyle trackColorFrom: self) color) ].
	self assureExtension.
	self extension borderStyle: newStyle.
	self changed
]

{ #category : 'accessing' }
Morph >> borderStyleForSymbol: aStyleSymbol [
	"Answer a suitable BorderStyle for me of the type represented by a given symbol"

	| aStyle existing |
	aStyle := BorderStyle borderStyleForSymbol: aStyleSymbol asSymbol.
	aStyle ifNil: [self error: 'bad style'].
	existing := self borderStyle.
	aStyle width: existing width;
		baseColor: existing baseColor.
	^ (self canDrawBorder: aStyle)
		ifTrue:
			[aStyle]
		ifFalse:
			[nil]
]

{ #category : 'accessing' }
Morph >> borderWidth [
	^self borderStyle width
]

{ #category : 'accessing' }
Morph >> borderWidthForRounding [

	^ self borderWidth
]

{ #category : 'geometry' }
Morph >> bottom [
	" Return the y-coordinate of my bottom side "

	^ bounds bottom
]

{ #category : 'geometry' }
Morph >> bottom: aNumber [
	" Move me so that my bottom is at the y-coordinate aNumber. My extent (width & height) are unchanged "

	self position: (bounds left @ (aNumber - self height))
]

{ #category : 'geometry' }
Morph >> bottomCenter [

	^ bounds bottomCenter
]

{ #category : 'geometry' }
Morph >> bottomLeft [

	^ bounds bottomLeft
]

{ #category : 'geometry' }
Morph >> bottomLeft: aPoint [
	" Move me so that my bottom left corner is at aPoint. My extent (width & height) are unchanged "

	self position: ((aPoint x) @ (aPoint y - self height))
]

{ #category : 'geometry' }
Morph >> bottomRight [

	^ bounds bottomRight
]

{ #category : 'geometry' }
Morph >> bottomRight: aPoint [
	" Move me so that my bottom right corner is at aPoint. My extent (width & height) are unchanged "

	self position: ((aPoint x - bounds width) @ (aPoint y - self height))
]

{ #category : 'drawing' }
Morph >> boundingBoxOfSubmorphs [
	| aBox visibleSubmorphs |
	visibleSubmorphs := submorphs select: [ :m | m visible ].
	visibleSubmorphs ifEmpty: [ ^ bounds origin extent: self minimumExtent "so won't end up with something empty" ].
	aBox := visibleSubmorphs first fullBounds.
	visibleSubmorphs allButFirst do: [ :m | aBox := aBox quickMerge: m fullBounds ].
	^ aBox
]

{ #category : 'geometry' }
Morph >> bounds [
	"Return the bounds of this morph."
	"Note: It is best not to override this method because many methods in Morph and its subclasses use the instance variable directly rather than 'self bounds'. Instead, subclasses should be sure that the bounds instance variable is correct."

	^ bounds
]

{ #category : 'geometry' }
Morph >> bounds: newBounds [
	| oldExtent newExtent |
	oldExtent := self extent.
	newExtent := newBounds extent.
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self position: newBounds topLeft; extent: newExtent.
	] ifFalse:[
		"We're shrinking. First resize then move."
		self extent: newExtent; position: newBounds topLeft.
	]
]

{ #category : 'geometry' }
Morph >> bounds: aRectangle from: referenceMorph [
	"Return the receiver's bounds as seen by aMorphs coordinate frame"
	owner ifNil: [^ aRectangle].
	^(owner transformFrom: referenceMorph) globalBoundsToLocal: aRectangle
]

{ #category : 'geometry' }
Morph >> bounds: aRectangle in: referenceMorph [
	"Return the receiver's bounds as seen by aMorphs coordinate frame"
	owner ifNil: [^ aRectangle].
	^(owner transformFrom: referenceMorph) localBoundsToGlobal: aRectangle
]

{ #category : 'halos and balloon help' }
Morph >> boundsForBalloon [

	"some morphs have bounds that are way too big"
	^self boundsInWorld
]

{ #category : 'geometry' }
Morph >> boundsIn: referenceMorph [
	"Return the receiver's bounds as seen by aMorphs coordinate frame"
	^self bounds: self bounds in: referenceMorph
]

{ #category : 'geometry' }
Morph >> boundsInWorld [
	^self bounds: self bounds in: self world
]

{ #category : 'updating' }
Morph >> breakDependents [

	self removeProperty: #announcer
]

{ #category : 'debug and other' }
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

{ #category : 'meta-actions' }
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

{ #category : 'meta-actions' }
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

{ #category : 'menu' }
Morph >> buildYellowButtonMenu: aHand [
	"build the morph menu for the yellow button"
	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	self addNestedYellowButtonItemsTo: menu event: self activeHand lastEvent.
	^ menu
]

{ #category : 'testing' }
Morph >> canDrawBorder: aBorderStyle [
	"Return true if the receiver can be drawn with the given border style."
	^true
]

{ #category : 'layout - properties' }
Morph >> cellInset [
	"Layout specific. This property specifies an extra inset for each cell in the layout."
	^ self layoutProperties ifNil:[0] ifNotNil: [:props | props cellInset]
]

{ #category : 'layout - properties' }
Morph >> cellInset: aNumber [
	"Layout specific. This property specifies an extra inset for each cell in the layout."
	self assureTableProperties cellInset: aNumber.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> cellPositioning [
	"Layout specific. This property describes how the receiver should be layed out in its owner when the bounds of the cell assigned to the receiver do not exactly match its bounds. Possible values are:
		#topLeft, #topRight, #bottomLeft, #bottomRight, #topCenter, #leftCenter, #rightCenter, #bottomCenter, #center
	which align the receiver's bounds with the cell at the given point."
	| props |
	props := self layoutProperties.
	^props ifNil:[#center] ifNotNil:[props cellPositioning]
]

{ #category : 'layout - properties' }
Morph >> cellPositioning: aSymbol [
	"Layout specific. This property describes how the receiver should be layed out in its owner when the bounds of the cell assigned to the receiver do not exactly match its bounds. Possible values are:
		#topLeft, #topRight, #bottomLeft, #bottomRight, #topCenter, #leftCenter, #rightCenter, #bottomCenter, #center
	which align the receiver's bounds with the cell at the given point."
	self assureTableProperties cellPositioning: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> cellPositioningString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self cellPositioning
]

{ #category : 'layout - properties' }
Morph >> cellSpacing [
	"Layout specific. This property describes how the cell size for each element in a list should be computed.
		#globalRect - globally equal rectangular cells
		#globalSquare - globally equal square cells
		#localRect - locally (e.g., per row/column) equal rectangular cells
		#localSquare - locally (e.g., per row/column) equal square cells
		#none - cells are sized based on available row/column constraints
	"
	| props |
	props := self layoutProperties.
	^props ifNil:[#none] ifNotNil:[props cellSpacing]
]

{ #category : 'layout - properties' }
Morph >> cellSpacing: aSymbol [
	"Layout specific. This property describes how the cell size for each element in a list should be computed.
		#globalRect - globally equal rectangular cells
		#globalSquare - globally equal square cells
		#localRect - locally (e.g., per row/column) equal rectangular cells
		#localSquare - locally (e.g., per row/column) equal square cells
		#none - cells are sized based on available row/column constraints
	"
	self assureTableProperties cellSpacing: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> cellSpacingString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self cellSpacing
]

{ #category : 'geometry' }
Morph >> center [

	^ bounds center
]

{ #category : 'geometry' }
Morph >> center: aPoint [
	self position: (aPoint - (self extent // 2))
]

{ #category : 'layout - menu' }
Morph >> changeClipLayoutCells [
	self invalidRect: self fullBounds.
	self clipLayoutCells: self clipLayoutCells not.
	self invalidRect: self fullBounds
]

{ #category : 'drawing' }
Morph >> changeClipSubmorphs [
	self clipSubmorphs: self clipSubmorphs not
]

{ #category : 'meta-actions' }
Morph >> changeColorTarget: anObject selector: aSymbol originalColor: aColor hand: aHand [
	"Put up a color picker for changing some kind of color."

	(self morphicUIManager chooseColor: aColor)
		ifNotNil: [:nc | anObject perform: aSymbol with: nc]
]

{ #category : 'menus' }
Morph >> changeDirectionHandles [
	^self wantsDirectionHandles: self wantsDirectionHandles not
]

{ #category : 'layout - menu' }
Morph >> changeDisableTableLayout [
	self disableTableLayout: self disableTableLayout not.
	self layoutChanged
]

{ #category : 'text-anchor' }
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

{ #category : 'menus' }
Morph >> changeDragAndDrop [
	^self enableDragNDrop: self dragNDropEnabled not
]

{ #category : 'text-anchor' }
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

{ #category : 'layout - menu' }
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

{ #category : 'layout - menu' }
Morph >> changeNoLayout [
	self layoutPolicy ifNil:[^self]. "already no layout"
	self layoutPolicy: nil.
	self layoutChanged
]

{ #category : 'text-anchor' }
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

{ #category : 'layout - menu' }
Morph >> changeReverseCells [
	self reverseTableCells: self reverseTableCells not
]

{ #category : 'layout - menu' }
Morph >> changeRubberBandCells [
	self rubberBandCells: self rubberBandCells not
]

{ #category : 'drop shadows' }
Morph >> changeShadowColor [
	"Change the shadow color of the receiver -- triggered, e.g. from a menu"

	(self morphicUIManager chooseColor: self shadowColor)
		ifNotNil: [:nc | self shadowColor: nc]
]

{ #category : 'recategorized' }
Morph >> changed [
	"Report that the area occupied by this morph should be redrawn.
	Fixed to include submorphs outside the outerBounds."

	^fullBounds
		ifNil: [self invalidRect: self privateFullBounds]
		ifNotNil: [self invalidRect: fullBounds]
]

{ #category : 'updating' }
Morph >> changed: anAspect [

	self doAnnounce: (MorphChanged new
		morph: self;
		selector: anAspect)
]

{ #category : 'updating' }
Morph >> changed: anAspect with: anObject [

	self doAnnounce: (MorphChangedWithArguments new
		morph: self;
		selector: anAspect;
		arguments: anObject)
]

{ #category : 'base - worlds' }
Morph >> clearArea [
	"Answer the clear area of the receiver. It means the area free
	of docking bars."
	| visTop visBottom visLeft visRight |

	visTop := self top.
	visBottom := self bottom.
	visLeft := self left.
	visRight := self right.

	self dockingBars
		do: [:each |
			(each isAdheringToTop and: [each bottom > visTop])
				ifTrue: [visTop := each bottom].

			(each isAdheringToBottom and: [each top < visBottom])
				ifTrue: [visBottom := each top].

			(each isAdheringToLeft and: [each right > visLeft])
				ifTrue: [visLeft := each right].

			(each isAdheringToRight and: [each left < visRight])
				ifTrue: [visRight := each left]
		].

	^ Rectangle
		left: visLeft
		right: visRight
		top: visTop
		bottom: visBottom
]

{ #category : 'event handling' }
Morph >> click [
	"Pretend the user clicked on me."

	(self handlesMouseDown: nil) ifTrue: [
		self mouseDown: nil.
		self mouseUp: nil]
]

{ #category : 'event handling' }
Morph >> click: evt [
	"Handle a single-click event. This message is only sent to clients that request it by sending #waitForClicksOrDrag:event: to the initiating hand in their mouseDown: method. This default implementation does nothing."

	^ self eventHandler ifNotNil:
		[self eventHandler click: evt fromMorph: self]
]

{ #category : 'drawing' }
Morph >> clipLayoutCells [
	"Drawing/layout specific. If this property is set, clip the
	submorphs of the receiver by its cell bounds."
	^ self
		valueOfProperty: #clipLayoutCells
		ifAbsent: [false]
]

{ #category : 'drawing' }
Morph >> clipLayoutCells: aBool [
	"Drawing/layout specific. If this property is set, clip the submorphs of the receiver by its cell bounds."
	aBool == false
		ifTrue:[self removeProperty: #clipLayoutCells]
		ifFalse:[self setProperty: #clipLayoutCells toValue: aBool].
	self changed
]

{ #category : 'drawing' }
Morph >> clipSubmorphs [
	"Drawing specific. If this property is set, clip the receiver's
	submorphs to the receiver's clipping bounds."

	extension ifNil: [^false].
	^extension clipSubmorphs ifNil: [false]
]

{ #category : 'drawing' }
Morph >> clipSubmorphs: aBool [
	"Drawing specific. If this property is set, clip the receiver's submorphs to the receiver's clipping bounds."
	self invalidRect: self fullBounds.

	aBool == self clipSubmorphs ifFalse:[
		self assureExtension.
		extension clipSubmorphs: aBool.
		self invalidRect: self fullBounds]
]

{ #category : 'printing' }
Morph >> clipText [
	"Copy the text in the receiver or in its submorphs to the clipboard"
	| content |
	"My own text"
	content := self userString.
	"Or in my submorphs"
	content ifNil: [
		| list |
		list := self allStringsAfter: nil.
		list notEmpty ifTrue: [
			content := String streamContents: [:stream |
				list do: [:each | stream nextPutAll: each; cr]]]].
	"Did we find something?"
	content
		ifNil: [self flash "provide feedback"]
		ifNotNil: [Clipboard clipboardText: content]
]

{ #category : 'drawing' }
Morph >> clippingBounds [
	"Return the bounds to which any submorphs should be clipped if the property is set"
	^self innerBounds
]

{ #category : 'settings' }
Morph >> cmdGesturesEnabled [
	^ self class cmdGesturesEnabled
]

{ #category : 'accessing' }
Morph >> color [

	^ color 	"has already been set to ((self valueOfProperty: #fillStyle) asColor)"
]

{ #category : 'accessing' }
Morph >> color: aColor [
	"Set the receiver's color.  Directly set the color if appropriate, else go by way of fillStyle"

	(aColor isColor or: [aColor isKindOf: InfiniteForm]) ifFalse:[^ self fillStyle: aColor].
	color = aColor ifFalse:
		[self assureExtension.
		extension fillStyle: nil.
		color := aColor.
		self changed]
]

{ #category : 'accessing' }
Morph >> colorForInsets [
	"Return the color to be used for shading inset borders.  The default is my own color, but it might want to be, eg, my owner's color.  Whoever's color ends up prevailing, the color itself gets the last chance to determine, so that when, for example, an InfiniteForm serves as the color, callers won't choke on some non-Color object being returned"
	(color isColor and:[color isTransparent and:[owner isNotNil]]) ifTrue:[^owner colorForInsets].
	^ color colorForInsets
]

{ #category : 'submorphs - add/remove' }
Morph >> comeToFront [
	| outerMorph |
	outerMorph := self topRendererOrSelf.
	(outerMorph owner isNil or: [outerMorph owner hasSubmorphs not])
		ifTrue: [^self].
	outerMorph owner firstSubmorph == outerMorph
		ifFalse: [outerMorph owner addMorphFront: outerMorph]
]

{ #category : 'halos and balloon help' }
Morph >> comeToFrontAndAddHalo [
	self comeToFront.
	self addHalo
]

{ #category : 'layout' }
Morph >> computeBounds [
	[ self doLayoutIn: self layoutBounds ]
		on: Error
		do: [ :ex |
			"This should do it unless you don't screw up the bounds"
			fullBounds := bounds.
			ex pass ]
]

{ #category : 'layout' }
Morph >> computeFullBounds [
	"turn off error handler to track down https://github.com/pharo-project/pharo/issues/12502"
	self doLayoutIn: self layoutBounds
	"
	[ self doLayoutIn: self layoutBounds ]
		on: Error
		do: [ :ex |
			This should do it unless you don't screw up the bounds
			fullBounds := bounds.
			ex pass ]"
]

{ #category : 'geometry testing' }
Morph >> containsPoint: aPoint [

	^ self bounds containsPoint: aPoint
]

{ #category : 'events - processing' }
Morph >> containsPoint: aPoint event: anEvent [
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	(self fullBounds containsPoint: aPoint) ifFalse:[^false].
	(self containsPoint: aPoint) ifTrue:[^true].
	self submorphsDo:[:m|
		(m isLocked and:[m fullContainsPoint:
			((m transformedFrom: self) globalPointToLocal: aPoint)]) ifTrue:[^true]].
	^false
]

{ #category : 'copying' }
Morph >> copy [

	^ self veryDeepCopy
]

{ #category : 'meta-actions' }
Morph >> copyToPasteBuffer: evt [
	^evt hand copyToPasteBuffer: self
]

{ #category : 'submorphs - add/remove' }
Morph >> copyWithoutSubmorph: sub [
	"Needed to get a morph to draw without one of its submorphs.
	NOTE:  This must be thrown away immediately after use."
	^ self shallowCopy privateSubmorphs: (submorphs copyWithout: sub)
]

{ #category : 'visual properties' }
Morph >> cornerStyle [
	"Returns one of the following symbols:
		#square
		#rounded
	according to the current corner style."

	^extension
		ifNil: [ #square ]
		ifNotNil: [ extension cornerStyle ]
]

{ #category : 'rounding' }
Morph >> cornerStyle: aSymbol [
	"This method makes it possible to set up desired corner style. aSymbol has to be one of:
		#square
		#rounded"

	aSymbol == self cornerStyle ifFalse:[
		self assureExtension.
		extension cornerStyle: aSymbol.
		self changed]
]

{ #category : 'accessing' }
Morph >> couldHaveRoundedCorners [
	^ true
]

{ #category : 'event handling' }
Morph >> cursorPoint [
	^ self currentHand lastEvent cursorPoint
]

{ #category : 'debug and other' }
Morph >> debugDrawError [

	(self valueOfProperty: #drawError) debug
]

{ #category : 'copying' }
Morph >> deepCopy [

	self error: 'Please use veryDeepCopy'
]

{ #category : 'accessing - defaults' }
Morph >> defaultBackgroundColor [
	"Answer the color to be used as the base window color for a window whose model is an object of the receiver's class"

	^  self theme windowColorFor: self
]

{ #category : 'accessing - defaults' }
Morph >> defaultBounds [
"answer the default bounds for the receiver"
	^ 0 @ 0 corner: 50 @ 40
]

{ #category : 'accessing - defaults' }
Morph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Color blue
]

{ #category : 'events - processing' }
Morph >> defaultEventDispatcher [
	"Return the default event dispatcher to use with events that are directly sent to the receiver"
 	^ self class defaultEventDispatcher
]

{ #category : 'settings' }
Morph >> defaultYellowButtonMenuEnabled [
	^ self class defaultYellowButtonMenuEnabled
]

{ #category : 'deferred message' }
Morph >> defer: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	self owner
		ifNotNil: [ self owner defer: aValuable]
		ifNil: [ self morphicUIManager defer: aValuable ]
]

{ #category : 'halos and balloon help' }
Morph >> defersHaloOnClickTo: aSubMorph [
	"If a cmd-click on aSubMorph would make it a preferred recipient of the halo, answer true"
	"May want to add a way (via a property) for morphs to assert true here -- this would let certain kinds of morphs that are unusually reluctant to take the halo on initial click"

	^ false
]

{ #category : 'rotate scale and flex' }
Morph >> degreesOfFlex [
	"Return any rotation due to flexing"
	"NOTE: because renderedMorph, which is used by the halo to set heading, goes down through dropShadows as well as transformations, we need this method (and its other implems) to come back up through such a chain."
	^ 0.0
]

{ #category : 'submorphs - add/remove' }
Morph >> delete [
	"Remove the receiver as a submorph of its owner and make its
	new owner be nil."

	self removeHalo.
	self activeHand ifNotNil: [ :hand |
		hand
			releaseKeyboardFocus: self;
			releaseMouseFocus: self.
	].
	owner ifNotNil:[
		self privateDelete.
		self announceDeleted.
	]
]

{ #category : 'halos and balloon help' }
Morph >> deleteBalloon [
	"If I am showing a balloon, delete it."
	| w |
	w := self world ifNil:[^self].
	w deleteBalloonTarget: self
]

{ #category : 'submorphs - add/remove' }
Morph >> deleteDockingBars [
	"Delete the receiver's docking bars"
	self dockingBars
		do: [:each | each delete]
]

{ #category : 'dropping/grabbing' }
Morph >> disableDragNDrop [
	self enableDragNDrop: false
]

{ #category : 'layout - properties' }
Morph >> disableTableLayout [
	"Layout specific. Disable laying out the receiver in table layout"
	| props |
	props := self layoutProperties.
	^props ifNil:[false] ifNotNil:[props disableTableLayout]
]

{ #category : 'layout - properties' }
Morph >> disableTableLayout: aBool [
	"Layout specific. Disable laying out the receiver in table layout"
	self assureLayoutProperties disableTableLayout: aBool.
	self layoutChanged
]

{ #category : 'meta-actions' }
Morph >> dismissMorph [
	"This is called from an explicit halo destroy/delete action."

	| w |
	w := self world ifNil:[^self].
	w stopStepping: self.
	self delete
]

{ #category : 'meta-actions' }
Morph >> dismissMorph: evt [
	self dismissMorph
]

{ #category : 'submorphs - add/remove' }
Morph >> dismissViaHalo [
	"The user has clicked in the delete halo-handle.  This provides a hook in case some concomitant action should be taken, or if the particular morph is not one which should be put in the trash can, for example."

	self setProperty: #lastPosition toValue: self positionInWorld.
	self dismissMorph
]

{ #category : 'change reporting' }
Morph >> displayExtentChanged [

	"
	This method is sent to all submorphs of the worldMorph when the external display extent changes (resizing the vm window).
	"
]

{ #category : 'settings' }
Morph >> displayScaleFactor [

	^ self currentWorld displayScaleFactor
]

{ #category : 'announcements' }
Morph >> doAnnounce: anAnnouncement [
	"Take care of not creating the announcer when announcing. If the announcer doesn't exist then this means nobody has expressed an interest in the message."

	"Do not override announce: for now, there is a need to refactor the announcements code in at least SystemWindow and ExpanderMorph."

	(self valueOfProperty: #announcer ifAbsent: [ ^ self ]) announce: anAnnouncement
]

{ #category : 'button' }
Morph >> doButtonAction [
	"If the receiver has a button-action defined, do it now.  The default button action of any morph is, well, to do nothing.  Note that there are several ways -- too many ways -- for morphs to have button-like actions.  This one refers not to the #mouseUpCodeToRun feature, nor does it refer to the Player-scripting mechanism.  Instead it is intended for morph classes whose very nature is to be buttons -- this method provides glue so that arbitrary buttons on the UI can be 'fired' programatticaly from user scripts"
]

{ #category : 'user interface' }
Morph >> doFastReframe: ptName [

	| newBounds |
	"For fast display, only higlight the rectangle during loop"
	newBounds := self boundsInWorld
		             newRectButtonPressedDo: [ :rectangle :cursorPoint |
			             rectangle
				             withSideOrCorner: ptName
				             setToPoint: cursorPoint
				             minExtent: self minimumExtent ]
		             inWorld: self world.
	self bounds: newBounds.

	^ newBounds
]

{ #category : 'layout' }
Morph >> doLayoutIn: layoutBounds [
	"Compute a new layout based on the given layout bounds."

	"Note: Testing for #bounds or #layoutBounds would be sufficient to
	figure out if we need an invalidation afterwards but #outerBounds
	is what we need for all leaf nodes so we use that."

	| box priorBounds |
	priorBounds := self outerBounds.
	submorphs isEmpty ifTrue: [^fullBounds := priorBounds].
	"Send #ownerChanged to our children"
	submorphs do: [:m | m ownerChanged].
	self layoutPolicy ifNotNil: [:layout | layout layout: self in: layoutBounds].
	self adjustLayoutBounds.
	fullBounds := self privateFullBounds.
	box := self outerBounds.
	box = priorBounds
		ifFalse: [self invalidRect: (priorBounds quickMerge: box)]
]

{ #category : 'opening' }
Morph >> doOpenInWorld: aWorld [

	aWorld addMorph: self
]

{ #category : 'submorphs - accessing' }
Morph >> dockingBars [
	"Answer the receiver's dockingBars"
	^ self submorphs
		select: [:each | each isDockingBar]
]

{ #category : 'accessing' }
Morph >> doesBevels [
	"To return true means that this object can show bevelled borders, and
	therefore can accept, eg, #raised or #inset as valid borderColors.
	Must be overridden by subclasses that do not support bevelled borders."

	^ false
]

{ #category : 'drawing' }
Morph >> doesOwnRotation [
	"Some morphs don't want to TransformMorph to rotate their images, but we do"
	^ false
]

{ #category : 'event handling' }
Morph >> doubleClick: evt [
	"Handle a double-click event. This message is only sent to clients that request it by sending #waitForClicksOrDrag:event: to the initiating hand in their mouseDown: method. This default implementation does nothing."
	"We should not double click if not on the same element"
	(self containsPoint: evt position) ifFalse: [ ^self ].
	^ self eventHandler ifNotNil:
		[self eventHandler doubleClick: evt fromMorph: self]
]

{ #category : 'event handling' }
Morph >> doubleClickTimeout: evt [
	"Handle a double-click timeout event. This message is only sent to clients that request it by sending #waitForClicksOrDrag:event: to the initiating hand in their mouseDown: method. This default implementation does nothing."

	^ self eventHandler ifNotNil:
		[self eventHandler doubleClickTimeout: evt fromMorph: self]
]

{ #category : 'dropping/grabbing' }
Morph >> dragEnabled [
	"Get this morph's ability to add and remove morphs via drag-n-drop."
	^(self valueOfProperty: #dragEnabled) == true
]

{ #category : 'dropping/grabbing' }
Morph >> dragEnabled: aBool [
	^self enableDrag: aBool
]

{ #category : 'dropping/grabbing' }
Morph >> dragNDropEnabled [
	"Note: This method is only useful for dragEnabled == dropEnabled at all times"
	self separateDragAndDrop.
	^self dragEnabled and:[self dropEnabled]
]

{ #category : 'dropping/grabbing' }
Morph >> dragSelectionColor [
	^ Color magenta
]

{ #category : 'drawing' }
Morph >> drawDropHighlightOn: aCanvas [
	self highlightedForDrop ifTrue: [
		aCanvas frameRectangle: self fullBounds color: self dropHighlightColor]
]

{ #category : 'drawing' }
Morph >> drawDropShadowOn: aCanvas [

	aCanvas
		translateBy: self shadowOffset
		during: [ :shadowCanvas |
			shadowCanvas roundShadowCornersOf: self during: [
				(shadowCanvas isVisible: self bounds) ifTrue: [
					shadowCanvas fillRectangle: self bounds fillStyle: self shadowColor
				]
			]
		]
]

{ #category : 'drawing' }
Morph >> drawErrorOn: aCanvas [
	"The morph (or one of its submorphs) had an error in its drawing method."
	aCanvas
		frameAndFillRectangle: bounds
		fillColor: Color red
		borderWidth: 1
		borderColor: Color yellow.
	aCanvas line: bounds topLeft to: bounds bottomRight width: 1 color: Color yellow.
	aCanvas line: bounds topRight to: bounds bottomLeft width: 1 color: Color yellow.

	self valueOfProperty: #drawError ifPresentDo: [ :error| | trace stringBounds |
		trace := String streamContents: [ :s| error signalerContext shortDebugStackOn: s].
		stringBounds := bounds insetBy: 5.
		trace linesDo: [ :aString|
			aCanvas drawString: aString in: stringBounds.
			stringBounds := stringBounds top: stringBounds top + (TextStyle defaultFont pixelSize * 1.2) ]]
]

{ #category : 'drawing' }
Morph >> drawMouseDownHighlightOn: aCanvas [
	self highlightedForMouseDown ifTrue: [
		aCanvas frameRectangle: self fullBounds color: self color darker darker]
]

{ #category : 'drawing' }
Morph >> drawOn: aCanvas [

	aCanvas fillRectangle: self bounds fillStyle: self fillStyle borderStyle: self borderStyle
]

{ #category : 'drawing' }
Morph >> drawSubmorphsOn: aCanvas [
	"Display submorphs back to front"

	| drawBlock |
	submorphs isEmpty ifTrue: [^self].
	drawBlock := [:canvas | submorphs reverseDo: [:m | canvas fullDrawMorph: m]].
	self clipSubmorphs
		ifTrue: [aCanvas clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock]
		ifFalse: [drawBlock value: aCanvas]
]

{ #category : 'dropping/grabbing' }
Morph >> dropEnabled [
	"Get this morph's ability to add and remove morphs via drag-n-drop."
	^(self valueOfProperty: #dropEnabled) == true
]

{ #category : 'dropping/grabbing' }
Morph >> dropEnabled: aBool [
	^self enableDrop: aBool
]

{ #category : 'event handling' }
Morph >> dropFiles: anEvent [
	"Handle a number of files dropped from the OS"
]

{ #category : 'dropping/grabbing' }
Morph >> dropHighlightColor [
	^ Color blue
]

{ #category : 'copying' }
Morph >> duplicate [
	"Make and return a duplicate of the receiver"

	| newMorph topRend |
	((topRend := self topRendererOrSelf) ~~ self) ifTrue: [^ topRend duplicate].
	newMorph := self veryDeepCopy.
	newMorph arrangeToStartStepping.
		newMorph privateOwner: nil. "no longer in world"
	^newMorph
]

{ #category : 'meta-actions' }
Morph >> duplicateMorph: evt [
	"Make and return a duplicate of the receiver's argument"
	| dup |
	dup := self duplicate.
	evt hand grabMorph: dup from: owner. "duplicate was ownerless so use #grabMorph:from: here"
	^dup
]

{ #category : 'halos and balloon help' }
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

{ #category : 'halos and balloon help' }
Morph >> editBalloonHelpText [
	"Modify the receiver's balloon help text."

	self editBalloonHelpContent: self balloonText
]

{ #category : 'accessing' }
Morph >> embeddedWindowOrNil [

	"answer nil for common morphs, yourself from system windows and first submorph for transformation morphs"

	^ nil
]

{ #category : 'dropping/grabbing' }
Morph >> enableDrag: aBoolean [
	self setProperty: #dragEnabled toValue: aBoolean
]

{ #category : 'dropping/grabbing' }
Morph >> enableDragNDrop [
	self enableDragNDrop: true
]

{ #category : 'dropping/grabbing' }
Morph >> enableDragNDrop: aBoolean [
	"Set both properties at once"
	self separateDragAndDrop.
	self enableDrag: aBoolean.
	self enableDrop: aBoolean
]

{ #category : 'dropping/grabbing' }
Morph >> enableDrop: aBoolean [
	self setProperty: #dropEnabled toValue: aBoolean
]

{ #category : 'accessing' }
Morph >> enabled [
	"Answer whether the receiver is enabled."

	^true
]

{ #category : 'accessing' }
Morph >> enabled: aBoolean [
	"does nothing"
]

{ #category : 'accessing' }
Morph >> eventHandler [
	"answer the receiver's eventHandler"
	^ extension ifNotNil: [extension eventHandler]
]

{ #category : 'accessing' }
Morph >> eventHandler: anEventHandler [
	"Note that morphs can share eventHandlers and all is OK. "
	self assureExtension eventHandler: anEventHandler
]

{ #category : 'drawing' }
Morph >> expandFullBoundsForRolloverBorder: aRectangle [
	| delta |
	delta := self valueOfProperty: #rolloverWidth ifAbsent: [10@10].
	^aRectangle expandBy: delta
]

{ #category : 'accessing - extension' }
Morph >> extension [
	"answer the recevier's extension"
	^ extension
]

{ #category : 'geometry' }
Morph >> extent [

	^ bounds extent
]

{ #category : 'geometry' }
Morph >> extent: aPoint [
	|newExtent|
	newExtent := aPoint rounded.
	(bounds extent closeTo: newExtent) ifTrue: [^ self].
	self changed.
	bounds := (bounds topLeft extent: newExtent).
	self layoutChanged.
	self changed
]

{ #category : 'viewer' }
Morph >> externalName [
	^ self assureExtension externalName ifNil: [self printString]
]

{ #category : 'visual properties' }
Morph >> fillStyle [
	"Return the current fillStyle of the receiver."

	^extension
		ifNil: [^color]
		ifNotNil: [extension fillStyle ifNil: [color]]
]

{ #category : 'visual properties' }
Morph >> fillStyle: aFillStyle [
	"Set the current fillStyle of the receiver.
	Optimized for no change."

	self assureExtension.
	extension fillStyle = aFillStyle
		ifTrue: [^self]. "no change optimization"
	extension fillStyle: aFillStyle.
	color := aFillStyle asColor.
	self changed
]

{ #category : 'submorphs - accessing' }
Morph >> findA: aClass [
	"Return the first submorph of the receiver that is descended from the given class. Return nil if there is no such submorph. Clients of this code should always check for a nil return value so that the code will be robust if the user takes the morph apart."

	^self submorphs
		detect: [:p | p isKindOf: aClass]
		ifNone: [nil]
]

{ #category : 'submorphs - accessing' }
Morph >> findDeepSubmorphThat: block1 ifAbsent: block2 [
	self
		allMorphsDo: [:m | (block1 value: m)
				== true ifTrue: [^ m]].
	^ block2 value
]

{ #category : 'submorphs - accessing' }
Morph >> findDeeplyA: aClass [
	"Return a morph in the submorph tree of the receiver that is descended from the given class. Return nil if there is no such morph. Clients of this code should always check for a nil return value so that the code will be robust if the user takes the morph apart."

	^ (self allMorphs copyWithout: self)
		detect: [:p | p isKindOf: aClass]
		ifNone: [nil]
]

{ #category : 'submorphs - accessing' }
Morph >> findSubmorphBinary: aBlock [
	"Use binary search for finding a specific submorph of the receiver. Caller must be certain that the ordering holds for the submorphs."
	^submorphs findBinary: aBlock do: [ :found | found ] ifNone: [:a :b | ]
]

{ #category : 'structure' }
Morph >> firstOwnerSuchThat: conditionBlock [

	self allOwnersDo: [:m | (conditionBlock value: m) ifTrue: [^ m]].
	^ nil
]

{ #category : 'submorphs - accessing' }
Morph >> firstSubmorph [
	^submorphs first
]

{ #category : 'geometry' }
Morph >> fitInWorld [

	self bounds: (self bounds translatedAndSquishedToBeWithin: self world bounds)
]

{ #category : 'macpal' }
Morph >> flash [

	self flashColor: self flashFillStyle
]

{ #category : 'macpal' }
Morph >> flashColor: aColor [
	| originalFill |

	originalFill := self fillStyle.

	self fillStyle: aColor.

	self addAlarm: #flashFinished:original: withArguments: { aColor . originalFill } after: 50
]

{ #category : 'macpal' }
Morph >> flashFillStyle [

	^Color black
]

{ #category : 'macpal' }
Morph >> flashFinished: flashFill original: originalFill [

	"if the color/fillstyle has changed while the flash is active, it will continue to use the new color/fill and not restrore the original"

	self fillStyle == flashFill ifTrue: [
		self fillStyle: originalFill
	]
]

{ #category : 'dropping/grabbing' }
Morph >> formerOwner [
	^self valueOfProperty: #formerOwner
]

{ #category : 'dropping/grabbing' }
Morph >> formerOwner: aMorphOrNil [
	aMorphOrNil ifNil: [self removeProperty: #formerOwner]
		ifNotNil: [self setProperty: #formerOwner toValue: aMorphOrNil]
]

{ #category : 'dropping/grabbing' }
Morph >> formerPosition [
	^self valueOfProperty: #formerPosition
]

{ #category : 'dropping/grabbing' }
Morph >> formerPosition: formerPosition [
	formerPosition
		ifNil: [self removeProperty: #formerPosition]
		ifNotNil: [self setProperty: #formerPosition toValue: formerPosition]
]

{ #category : 'layout' }
Morph >> fullBounds [
	"Return the bounding box of the receiver and all its children. Recompute the layout if necessary."

	fullBounds ifNotNil: [ ^ fullBounds ].	"Errors at this point can be critical so make sure we catch 'em all right"
	self computeFullBounds.	"This should do it unless you don't screw up the bounds"
	^ fullBounds
]

{ #category : 'geometry' }
Morph >> fullBoundsInWorld [
	^self bounds: self fullBounds in: self world
]

{ #category : 'geometry testing' }
Morph >> fullContainsPoint: aPoint [
	(self fullBounds containsPoint: aPoint)
		ifFalse: [ ^ false ].	"quick elimination"
	(self containsPoint: aPoint)
		ifTrue: [ ^ true ].	"quick acceptance"
	^ submorphs anySatisfy: [ :m | m fullContainsPoint: aPoint ]
]

{ #category : 'drawing' }
Morph >> fullDrawOn: aCanvas [
	"Draw the full Morphic structure on the given Canvas"

	self visible ifFalse: [^ self].
	(aCanvas isVisible: self fullBounds) ifFalse:[^self].

	(self hasProperty: #errorOnDraw) ifTrue:[^self drawErrorOn: aCanvas].

	[
		"Note: At some point we should generalize this into some sort of
		multi-canvas so that we can cross-optimize some drawing operations."
		"Pass 1: Draw eventual drop-shadow"
		self hasDropShadow ifTrue: [ self drawDropShadowOn: aCanvas ].

		"Pass 2: Draw receiver itself"

		aCanvas roundCornersOf: self during:[
			(aCanvas isVisible: self bounds) ifTrue:[aCanvas drawMorph: self].
			self drawSubmorphsOn: aCanvas.
			self drawDropHighlightOn: aCanvas.
			self drawMouseDownHighlightOn: aCanvas]

	] on: Error do: [:err |
		self setProperty: #errorOnDraw toValue: true.
		self setProperty: #drawError toValue: err freeze.
		^ self drawErrorOn: aCanvas
	]
]

{ #category : 'caching' }
Morph >> fullReleaseCachedState [
	"Release the cached state of the receiver and its full submorph tree."

	self allMorphsDo: [:m | m releaseCachedState]
]

{ #category : 'geometry' }
Morph >> globalPointToLocal: aPoint [
	^self point: aPoint from: nil
]

{ #category : 'submorphs - add/remove' }
Morph >> goBehind [

	owner addMorphBack: self
]

{ #category : 'geometry' }
Morph >> goHome [
	| box fb |
	owner ifNil: [^ self].
	self visible ifFalse: [^ self].

	box := owner visibleClearArea.
	fb := self fullBounds.

	fb left < box left
		ifTrue: [self left: box left - fb left + self left].
	fb right > box right
		ifTrue: [self right: box right - fb right + self right].

	fb top < box top
		ifTrue: [self top: box top - fb top + self top].
	fb bottom > box bottom
		ifTrue: [self bottom: box bottom - fb bottom + self bottom]
]

{ #category : 'meta-actions' }
Morph >> grabMorph: evt [

	evt hand grabMorph: self
]

{ #category : 'dropping/grabbing' }
Morph >> grabTransform [
	"Return the transform for the receiver which should be applied during grabbing"
	^owner ifNil:[IdentityTransform new] ifNotNil:[owner grabTransform]
]

{ #category : 'layout - properties' }
Morph >> hResizing [
	"Layout specific. This property describes how the receiver should be resized with respect to its owner and its children. Possible values are:
		#rigid			-	do not resize the receiver
		#spaceFill		-	resize to fill owner's available space
		#shrinkWrap	- resize to fit children
	"
	| props |
	props := self layoutProperties.
	^props ifNil:[#rigid] ifNotNil:[props hResizing]
]

{ #category : 'layout - properties' }
Morph >> hResizing: aSymbol [
	"Layout specific. This property describes how the receiver should be resized with respect to its owner and its children. Possible values are:
		#rigid			-	do not resize the receiver
		#spaceFill		-	resize to fill owner's available space
		#shrinkWrap	- resize to fit children
	"
	self assureLayoutProperties hResizing: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> hResizingString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self hResizing
]

{ #category : 'halos and balloon help' }
Morph >> halo [
	^ self primaryHand halo
		ifNotNil: [ :h | h target == self ifTrue: [ h ] ]
]

{ #category : 'halos and balloon help' }
Morph >> haloClass [
	"Answer the name of the desired kind of HaloMorph to launch on behalf of the receiver"

	^ #HaloMorph
]

{ #category : 'accessing' }
Morph >> halosEnabled [

	^ self class halosEnabled
]

{ #category : 'events - processing' }
Morph >> handleDropFiles: anEvent [
	"Handle a drop from the OS."
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(self wantsDropFiles: anEvent) ifFalse:[^self].
	anEvent wasHandled: true.
	self dropFiles: anEvent
]

{ #category : 'events - processing' }
Morph >> handleDropMorph: anEvent [
	"Handle a dropping morph."
	| aMorph localPt |
	aMorph := anEvent contents.
	"Do a symmetric check if both morphs like each other"
	((self wantsDroppedMorph: aMorph event: anEvent)	"I want her"
		and: [aMorph wantsToBeDroppedInto: self])		"she wants me"
		ifFalse: [^ self].
	anEvent wasHandled: true.
	"Transform the morph into the receiver's coordinate frame. This is currently incomplete since it only takes the offset into account where it really should take the entire transform."
	localPt := (self transformedFrom: anEvent hand world) "full transform down"
				globalPointToLocal: aMorph referencePosition.
	aMorph referencePosition: localPt.
	self acceptDroppingMorph: aMorph event: anEvent.
	aMorph justDroppedInto: self event: anEvent
]

{ #category : 'events - processing' }
Morph >> handleEvent: anEvent [
	"Handle the given event"
	^anEvent sentTo: self
]

{ #category : 'events - processing' }
Morph >> handleFocusEvent: anEvent [
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."
	^self handleEvent: anEvent
]

{ #category : 'events - processing' }
Morph >> handleKeyDown: anEvent [

	"System level event handling."

	anEvent wasHandled ifTrue: [ ^ self ].

	self shortcutsHandler ifNotNil: [ :handler |
		handler handleKeystroke: anEvent inMorph: self.
		anEvent wasHandled ifTrue: [
			anEvent supressNextKeyPress: true.
			^ self ] ].

	(self handlesKeyDown: anEvent) ifFalse: [ ^ self ].
	anEvent wasHandled: true.
	self keyDown: anEvent.
	^ self eventHandler ifNotNil: [ :handler |
		  handler keyDown: anEvent fromMorph: self ]
]

{ #category : 'events - processing' }
Morph >> handleKeyUp: anEvent [
	"System level event handling."

	anEvent wasHandled ifTrue:[ ^ self ].
	(self handlesKeyUp: anEvent) ifFalse:[ ^ self ].
	anEvent wasHandled: true.
	self keyUp: anEvent.
	^ self eventHandler ifNotNil: [ :handler | handler keyUp: anEvent fromMorph: self ]
]

{ #category : 'event handling' }
Morph >> handleKeystroke: anEvent [
	"System level event handling."

	anEvent wasHandled
		ifTrue: [^ self].

	(self handlesKeyStroke: anEvent)
		ifFalse: [^ self].
	anEvent wasHandled: true.
	self keyStroke: anEvent.
	^ self eventHandler ifNotNil: [:handler | handler keyStroke: anEvent fromMorph: self ]
]

{ #category : 'events - processing' }
Morph >> handleListenEvent: anEvent [
	"Handle the given event. This message is sent if the receiver is a registered listener for the given event."
	^anEvent sentTo: self
]

{ #category : 'events - processing' }
Morph >> handleMouseDown: anEvent [

	"System level event handling."

	anEvent wasHandled ifTrue: [ ^ self ]. "not interested"
	anEvent hand removePendingBalloonFor: self.
	anEvent wasHandled: true.

	(anEvent controlKeyPressed and: [
		 self cmdGesturesEnabled and: [ anEvent shiftPressed ] ]) ifTrue: [
		self invokeMetaMenu: anEvent.
		^ self eventHandler ifNotNil: [ :handler |
			  handler mouseDown: anEvent fromMorph: self ] ].

	"Make me modal during mouse transitions"
	anEvent hand newMouseFocus: self event: anEvent.

	"Check to open halo"
	anEvent isSpecialGesture
		ifTrue: [
			^ self handleSpecialGesture: anEvent ].

	self mouseDown: anEvent.

	anEvent hand removeHaloFromClick: anEvent on: self.
	(self handlesMouseStillDown: anEvent) ifTrue: [
		self
			startStepping: #handleMouseStillDown:
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: { anEvent copy resetHandlerFields }
			stepTime: self mouseStillDownStepRate ].

	^ self eventHandler ifNotNil: [ :handler |
		  handler mouseDown: anEvent fromMorph: self ]
]

{ #category : 'events - processing' }
Morph >> handleMouseEnter: anEvent [
	"System level event handling."

	anEvent isDraggingEvent
		ifTrue: [ (self handlesMouseOverDragging: anEvent)
				ifTrue: [ anEvent wasHandled: true.
					self mouseEnterDragging: anEvent ].
			^ self eventHandler ifNotNil: [ :handler | handler mouseEnterDragging: anEvent fromMorph: self ] ].
	self wantsBalloon ifTrue: [ anEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime ].

	(self handlesMouseOver: anEvent)
		ifTrue: [ anEvent wasHandled: true.
			self mouseEnter: anEvent ].

	self eventHandler ifNotNil: [ :handler | ^ handler mouseEnter: anEvent fromMorph: self ]
]

{ #category : 'events - processing' }
Morph >> handleMouseLeave: anEvent [
	"System level event handling."

	anEvent hand removePendingBalloonFor: self.
	anEvent isDraggingEvent
		ifTrue: [ (self handlesMouseOverDragging: anEvent)
				ifTrue: [ anEvent wasHandled: true.
					self mouseLeaveDragging: anEvent ].
			^ self eventHandler ifNotNil: [ :handler | handler mouseLeave: anEvent fromMorph: self ] ].
	(self handlesMouseOver: anEvent) ifTrue: [
			anEvent wasHandled: true.
			self mouseLeave: anEvent ].
	^ self eventHandler ifNotNil: [ :handler | handler mouseLeave: anEvent fromMorph: self ]
]

{ #category : 'events - processing' }
Morph >> handleMouseMove: anEvent [
	"System level event handling."
	anEvent wasHandled ifTrue:[^self].

	(self handlesMouseMove: anEvent) ifFalse: [ ^self ].

	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #handleMouseStillDown:
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields}
			stepTime: self mouseStillDownStepRate ].

	^ self eventHandler ifNotNil: [:handler | handler mouseMove: anEvent fromMorph: self ]
]

{ #category : 'events - processing' }
Morph >> handleMouseOver: anEvent [
	"System level event handling."
	anEvent hand mouseFocus == self ifTrue:[
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: anEvent position event: anEvent) ifFalse:[^self]].
	anEvent hand noticeMouseOver: self event: anEvent
]

{ #category : 'events - processing' }
Morph >> handleMouseStillDown: anEvent [
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	(anEvent hand mouseFocus == self)
		ifFalse:[^self stopSteppingSelector: #handleMouseStillDown:].
	self mouseStillDown: anEvent.
	^ self eventHandler ifNotNil: [:handler | handler mouseStillDown: anEvent fromMorph: self ]
]

{ #category : 'events - processing' }
Morph >> handleMouseUp: anEvent [
	"System level event handling."
	anEvent wasHandled ifTrue: [^self]. "not interested"
	anEvent hand mouseFocus == self ifFalse: [^self]. "Not interested in other parties"
	anEvent hand releaseMouseFocus: self.
	anEvent wasHandled: true.
	anEvent blueButtonChanged
		ifTrue: [
			self blueButtonUp: anEvent ]
		ifFalse: [ | result |
			result := self mouseUp: anEvent.
			self stopSteppingSelector: #handleMouseStillDown:.
			result ].
	^ self eventHandler ifNotNil: [:handler | handler mouseUp: anEvent fromMorph: self]
]

{ #category : 'events - processing' }
Morph >> handleMouseWheel: anEvent [
	"System level event handling."

	anEvent wasHandled ifTrue:[^self].
	(self handlesMouseWheel: anEvent) ifTrue:[
		anEvent wasHandled: true.
		self mouseWheel: anEvent]
]

{ #category : 'meta-actions' }
Morph >> handleSpecialGesture: mouseDownEvent [
	"Special gestures (cmd-mouse on the Macintosh; Alt-mouse on Windows and Unix) allow a mouse-sensitive morph to be moved or bring up a halo for the morph."
	| h tfm doNotDrag |
	h := mouseDownEvent hand halo.
	"Prevent wrap around halo transfers originating from throwing the event back in"
	doNotDrag := false.
	h ifNotNil:[
		(h innerTarget == self) ifTrue:[doNotDrag := true].
		(h innerTarget hasOwner: self) ifTrue:[doNotDrag := true].
		(self hasOwner: h target) ifTrue:[doNotDrag := true]].

	tfm := (self transformedFrom: nil) inverseTransformation.

	"cmd-drag on flexed morphs works better this way"
	h := self addHalo: (mouseDownEvent transformedBy: tfm).
	h ifNil: [^ self].
	doNotDrag ifTrue:[^self].
	"Initiate drag transition if requested"
	mouseDownEvent hand
		waitForClicksOrDrag: h
		event: (mouseDownEvent transformedBy: tfm)
		selectors: { nil. nil. nil. #dragTarget:. }
		threshold: 5.
	"Pass focus explicitly here"
	mouseDownEvent hand newMouseFocus: h
]

{ #category : 'event handling' }
Morph >> handleTextEditionEvent: anEvent [
	"System level event handling."

	anEvent wasHandled ifTrue: [ ^ self ].
	(self handlesTextEditionEvent: anEvent) ifTrue: [
		anEvent wasHandled: true.
		self textEdition: anEvent ]
]

{ #category : 'event handling' }
Morph >> handleTextInputEvent: anEvent [
	"System level event handling."

	anEvent wasHandled ifTrue:[^self].
	(self handlesTextInputEvent: anEvent) ifTrue:[
		anEvent wasHandled: true.
		self textInput: anEvent]
]

{ #category : 'events - processing' }
Morph >> handleUnknownEvent: anEvent [
	"An event of an unknown type was sent to the receiver. What shall we do?!"

	self inform: 'Unknown event: ' , anEvent printString.
	anEvent wasHandled: true
]

{ #category : 'updating' }
Morph >> handleUpdate: aMorphChangedAnnouncement [
	^ aMorphChangedAnnouncement deliverTo: self
]

{ #category : 'event handling' }
Morph >> handleWindowEvent: anEvent [
	"Handle an event concerning our host window"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(self wantsWindowEvent: anEvent) ifFalse:[^self].
	anEvent wasHandled: true.
	self windowEvent: anEvent
]

{ #category : 'meta-actions' }
Morph >> handlerForBlueButtonDown: anEvent [
	"Return the (prospective) handler for a mouse down event.
	In this case all blue buttons are disabled unless the subclass whant to handle it"

	^ nil
]

{ #category : 'event handling' }
Morph >> handlerForMouseDown: anEvent [
	"Return the (prospective) handler for a mouse down event. The handler is temporarily
	installed and can be used for morphs further down the hierarchy to negotiate whether
	the inner or the outer morph should finally handle the event."

	anEvent isSpecialGesture
		ifTrue: [ ^ self handlerForSpecialGestureDown: anEvent ].

	anEvent blueButtonPressed
		ifTrue: [^ self handlerForBlueButtonDown: anEvent].
	anEvent yellowButtonPressed
		ifTrue: [^ self handlerForYellowButtonDown: anEvent].
	(self handlesMouseDown: anEvent)
		ifFalse: [^ nil].	"not interested"

	anEvent handler
		ifNil: [^ self ].	"Same priority but I am innermost"

	"Nobody else was interested"
	^self mouseDownPriority >= anEvent handler mouseDownPriority
		ifTrue: [ self]
		ifFalse: [ nil]
]

{ #category : 'meta-actions' }
Morph >> handlerForSpecialGestureDown: anEvent [
	"Return the (prospective) handler for a special gesture event (Halos).
	The handler is temporarily installed and can be used for morphs further down the hierarchy to negotiate whether the inner or the outer morph should finally handle the event."

	self wantsHaloFromClick ifFalse: [ ^ nil ].

	self class cycleHalosBothDirections
		ifTrue: [ anEvent handler ifNil: [ ^ self ].
			(anEvent handler isKindOf: PasteUpMorph) ifTrue: [ ^ self ] ].

	^ self
]

{ #category : 'event handling' }
Morph >> handlerForYellowButtonDown: anEvent [
	"Return the (prospective) handler for a mouse down event with the yellow button pressed.
	The 	handler is temporarily installed and can be used for morphs further
	down the hierarchy to negotiate whether the inner or the outer
	morph should finally handle the event."

	(self hasYellowButtonMenu or: [ self handlesMouseDown: anEvent ])
		ifFalse: [ ^ nil].	"Not interested."

	anEvent handler
		ifNil: [^ self].	"Nobody else was interested"

	"Same priority but I am innermost."
	^ self mouseDownPriority >= anEvent handler mouseDownPriority
		ifFalse: [nil ]
		ifTrue: [self]
]

{ #category : 'recategorized' }
Morph >> handlesDropShadowInHand [
	"Answer whether the receiver will handle drop shadow drawing when picked up in the hand."

	^false
]

{ #category : 'event handling' }
Morph >> handlesKeyDown: evt [
	^self handlesKeyboard: evt
]

{ #category : 'event handling' }
Morph >> handlesKeyStroke: evt [
	^self handlesKeyboard: evt
]

{ #category : 'event handling' }
Morph >> handlesKeyUp: evt [
	^self handlesKeyboard: evt
]

{ #category : 'event handling' }
Morph >> handlesKeyboard: evt [
	"Return true if the receiver wishes to handle the given keyboard event"
	self eventHandler ifNotNil: [^ self eventHandler handlesKeyboard: evt].
	^ false
]

{ #category : 'event handling' }
Morph >> handlesMouseDown: evt [
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	"NOTE: The default response is false, except if you have added sensitivity to mouseDown events using the on:send:to: mechanism.  Subclasses that implement these messages directly should override this one to return true."

	self eventHandler ifNotNil: [^ self eventHandler handlesMouseDown: evt].
	^ false
]

{ #category : 'events - processing' }
Morph >> handlesMouseMove: anEvent [
	"Rules say that by default a morph gets #mouseMove if
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."

	self eventHandler ifNotNil: [ :handler |
		(handler handlesMouseMove: anEvent) ifTrue: [
			^ true ] ].

	^anEvent hand submorphs isEmpty and: [
		(anEvent anyButtonPressed and:[
			anEvent hand mouseFocus == self
		])]
]

{ #category : 'event handling' }
Morph >> handlesMouseOver: evt [
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false, except if you have added sensitivity to mouseEnter: or mouseLeave:, using the on:send:to: mechanism."

	self eventHandler ifNotNil: [^ self eventHandler handlesMouseOver: evt].
	^ false
]

{ #category : 'event handling' }
Morph >> handlesMouseOverDragging: evt [
	"Return true if I want to receive mouseEnterDragging: and mouseLeaveDragging: when the hand drags something over me (button up or button down), or when the mouse button is down but there is no mouseDown recipient.  The default response is false, except if you have added sensitivity to mouseEnterLaden: or mouseLeaveLaden:, using the on:send:to: mechanism."
	"NOTE:  If the hand state matters in these cases, it may be tested by constructs such as
		event anyButtonPressed
		event hand hasSubmorphs"

	self eventHandler ifNotNil: [^ self eventHandler handlesMouseOverDragging: evt].
	^ false
]

{ #category : 'event handling' }
Morph >> handlesMouseStillDown: evt [
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	self eventHandler ifNotNil: [^ self eventHandler handlesMouseStillDown: evt].
	^ false
]

{ #category : 'events - processing' }
Morph >> handlesMouseWheel: evt [
	"Do I want to receive mouseWheel events?."

	^false
]

{ #category : 'event handling' }
Morph >> handlesTextEditionEvent: evt [
	"Do I want to receive text edition events?"

	^false
]

{ #category : 'event handling' }
Morph >> handlesTextInputEvent: evt [
	"Do I want to receive text edition events?"

	^false
]

{ #category : 'layout - menu' }
Morph >> hasClipLayoutCellsString [
	^ (self clipLayoutCells) -> 'clip to cell size' translated
]

{ #category : 'drawing' }
Morph >> hasClipSubmorphsString [
	"Answer a string that represents the clip-submophs checkbox"
	^ (self clipSubmorphs) -> 'provide clipping' translated
]

{ #category : 'menus' }
Morph >> hasDirectionHandlesString [
	^ (self wantsDirectionHandles) ->  'direction handles' translated
]

{ #category : 'layout - menu' }
Morph >> hasDisableTableLayoutString [
	^ (self disableTableLayout) -> 'disable layout in tables' translated
]

{ #category : 'text-anchor' }
Morph >> hasDocumentAnchorString [
	^ (self textAnchorType == #document) ->  'Document' translated
]

{ #category : 'menus' }
Morph >> hasDragAndDropEnabledString [
	"Answer a string to characterize the drag & drop status of the
	receiver"
	^ (self dragNDropEnabled) -> 'accept drops' translated
]

{ #category : 'drop shadows' }
Morph >> hasDropShadow [

	"answer whether the receiver has DropShadow"

	^ extension
		  ifNil: [ false ]
		  ifNotNil: [ :ext | ext valueOfProperty: #hasDropShadow ifAbsent: false ]
]

{ #category : 'drop shadows' }
Morph >> hasDropShadow: aBool [
	aBool
		ifTrue:[self setProperty: #hasDropShadow toValue: true]
		ifFalse:[self removeProperty: #hasDropShadow]
]

{ #category : 'drop shadows' }
Morph >> hasDropShadowString [
	^ (self hasDropShadow) ->  'show shadow' translated
]

{ #category : 'accessing - extension' }
Morph >> hasExtension [
	"answer whether the receiver has extention"
	^ extension isNotNil
]

{ #category : 'event handling' }
Morph >> hasFocus [
	^ false
]

{ #category : 'halos and balloon help' }
Morph >> hasHalo [
	^self hasProperty: #hasHalo
]

{ #category : 'halos and balloon help' }
Morph >> hasHalo: aBool [
	aBool
		ifTrue:[self setProperty: #hasHalo toValue: true]
		ifFalse:[self removeProperty: #hasHalo]
]

{ #category : 'text-anchor' }
Morph >> hasInlineAnchorString [
	^ (self textAnchorType == #inline)-> 'Inline' translated
]

{ #category : 'layout - menu' }
Morph >> hasNoLayoutString [
	^ (self layoutPolicy isNil) -> 'no layout' translated
]

{ #category : 'structure' }
Morph >> hasOwner: aMorph [
	"Return true if the receiver has aMorph in its owner chain"
	aMorph ifNil:[^true].
	self allOwnersDo:[:m| m = aMorph ifTrue:[^true]].
	^false
]

{ #category : 'text-anchor' }
Morph >> hasParagraphAnchorString [
	^ (self textAnchorType == #paragraph) -> 'Paragraph' translated
]

{ #category : 'accessing - properties' }
Morph >> hasProperty: aSymbol [

	"Answer whether the receiver has the property named aSymbol"

	^ extension
		  ifNil: [ false ]
		  ifNotNil: [ :ext | ext hasProperty: aSymbol ]
]

{ #category : 'layout - menu' }
Morph >> hasProportionalLayoutString [
	| layout |
	^ ((layout := self layoutPolicy) isNotNil
			and: [layout isProportionalLayout]) -> 'proportional layout' translated
]

{ #category : 'layout - menu' }
Morph >> hasReverseCellsString [
	^ (self reverseTableCells) -> 'reverse table cells' translated
]

{ #category : 'layout - menu' }
Morph >> hasRubberBandCellsString [
	^ (self rubberBandCells) -> 'rubber band cells' translated
]

{ #category : 'submorphs - accessing' }
Morph >> hasSubmorphs [
	^submorphs notEmpty
]

{ #category : 'layout - menu' }
Morph >> hasTableLayoutString [
	| layout |
	^ ((layout := self layoutPolicy) isNotNil
			and: [layout isTableLayout]) ->   'table layout' translated
]

{ #category : 'menu' }
Morph >> hasYellowButtonMenu [
	"Answer true if I have any items at all for a context (yellow button) menu."
	^ self wantsYellowButtonMenu
]

{ #category : 'geometry' }
Morph >> height [

	^ bounds height
]

{ #category : 'geometry' }
Morph >> height: aNumber [
	" Set my height; my position (top-left corner) and width will remain the same "

	self extent: self width@aNumber asInteger
]

{ #category : 'base - widgets' }
Morph >> heightToDisplayInList: aList [

	^ self minExtent y
]

{ #category : 'drawing' }
Morph >> hide [
	owner ifNil: [ ^ self ].
	self visible
		ifFalse: [ ^ self ].
	self visible: false.
	self changed
]

{ #category : 'accessing' }
Morph >> highlight [
	"The receiver is being asked to appear in a highlighted state.  Mostly used for textual morphs"
	self color: self highlightColor
]

{ #category : 'accessing' }
Morph >> highlightColor [

	^ (self valueOfProperty: #highlightColor) ifNil: [
		  owner ifNil: [ self color ] ifNotNil: [ owner highlightColor ] ]
]

{ #category : 'accessing' }
Morph >> highlightColor: aColor [
	self setProperty: #highlightColor toValue: aColor
]

{ #category : 'dropping/grabbing' }
Morph >> highlightForDrop [
	self highlightForDrop: true
]

{ #category : 'dropping/grabbing' }
Morph >> highlightForDrop: aBoolean [
	self setProperty: #highlightedForDrop toValue: aBoolean.
	self changed
]

{ #category : 'drawing' }
Morph >> highlightForMouseDown [
	self highlightForMouseDown: true
]

{ #category : 'drawing' }
Morph >> highlightForMouseDown: aBoolean [
	aBoolean
		ifTrue:[self setProperty: #highlightedForMouseDown toValue: aBoolean]
		ifFalse:[self removeProperty: #highlightedForMouseDown. self resetExtension].
	self changed
]

{ #category : 'dropping/grabbing' }
Morph >> highlightedForDrop [
	^(self valueOfProperty: #highlightedForDrop) == true
]

{ #category : 'drawing' }
Morph >> highlightedForMouseDown [
	^(self valueOfProperty: #highlightedForMouseDown) == true
]

{ #category : 'thumbnail' }
Morph >> icon [
	"Answer a form with an icon to represent the receiver"
	^ self valueOfProperty: #icon
]

{ #category : 'thumbnail' }
Morph >> iconOrThumbnail [
	"Answer an appropiate form to represent the receiver"

	^ self icon
		ifNil: [ | maxExtent fb |maxExtent := 320 @ 240.
			fb := self fullBounds.
			fb area <= (maxExtent x * maxExtent y)
				ifTrue: [self imageForm]
				ifFalse: [self imageFormForRectangle: (fb topLeft extent: maxExtent)]
		]
]

{ #category : 'thumbnail' }
Morph >> iconOrThumbnailOfSize: aNumberOrPoint [
	"Answer an appropiate form to represent the receiver"

	^ self iconOrThumbnail scaledIntoFormOfSize: aNumberOrPoint
]

{ #category : 'drawing' }
Morph >> imageForm [

	^ self imageFormForRectangle: self fullBounds
]

{ #category : 'drawing' }
Morph >> imageForm: depth backgroundColor: aColor forRectangle: rect [

	| canvas |
	canvas := FormCanvas extent: rect extent depth: depth.
	canvas translateBy: rect topLeft negated during: [ :tempCanvas |
		tempCanvas fillRectangle: rect color: aColor.
		tempCanvas fullDrawMorph: self ].
	^ canvas form offset: rect topLeft
]

{ #category : 'drawing' }
Morph >> imageForm: depth forRectangle: rect [
	| canvas |
	canvas := FormCanvas extent: rect extent depth: depth.
	canvas translateBy: rect topLeft negated
		during:[:tempCanvas| tempCanvas fullDrawMorph: self].
	^ canvas form offset: rect topLeft
]

{ #category : 'drawing' }
Morph >> imageFormDepth: depth [

	^ self imageForm: depth forRectangle: self fullBounds
]

{ #category : 'drawing' }
Morph >> imageFormForRectangle: rect [

	^ self imageForm: 32 forRectangle: rect
]

{ #category : 'user interface' }
Morph >> initialExtent [

	 ^(self valueOfProperty: #initialExtent) ifNil: [ 700@500 ] ifNotNil: [ :ext | ext ]
]

{ #category : 'initialization' }
Morph >> initialize [
	"initialize the state of the receiver"

	super initialize.
	submorphs := EmptyArray.
	bounds := self defaultBounds.
	color := self defaultColor
]

{ #category : 'accessing - extension' }
Morph >> initializeExtension [
	"private - initializes the receiver's extension"
	extension := MorphExtension new
]

{ #category : 'geometry' }
Morph >> innerBounds [
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

	^ self bounds insetBy: self borderWidth
]

{ #category : 'accessing' }
Morph >> insetColor [

	^ owner ifNil: [ self color ] ifNotNil: [ self colorForInsets ]
]

{ #category : 'meta-actions' }
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

{ #category : 'menus' }
Morph >> inspectInMorphic: evt [
	Smalltalk tools inspector inspect: self
]

{ #category : 'debug and other' }
Morph >> inspectOwnerChain [
	self ownerChain inspectWithLabel: 'Owner chain for ', self printString
]

{ #category : 'geometry' }
Morph >> intersects: aRectangle [
	"Answer whether aRectangle, which is in World coordinates, intersects me."

	^self fullBoundsInWorld intersects: aRectangle
]

{ #category : 'initialize' }
Morph >> intoWorld: aWorld [
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	aWorld ifNil:[^self].
	self wantsSteps ifTrue:[aWorld startStepping: self].
	self submorphsDo:[:m| m intoWorld: aWorld]
]

{ #category : 'change reporting' }
Morph >> invalidRect: damageRect [
	^self invalidRect: damageRect from: self
]

{ #category : 'change reporting' }
Morph >> invalidRect: aRectangle from: aMorph [
	| damageRect |
	aRectangle hasPositiveExtent ifFalse: [ ^self ].
	damageRect := aRectangle.
	aMorph == self ifFalse:[
		"Clip to receiver's clipping bounds if the damage came from a child"
		self clipSubmorphs
			ifTrue:[damageRect := aRectangle intersect: self clippingBounds ifNone: [ ^ self ]]].
	owner ifNotNil: [owner invalidRect: damageRect from: self]
]

{ #category : 'meta-actions' }
Morph >> invokeMetaMenu: evt [
	| menu |
	self world ifNil: [ ^self ].

	menu := self buildMetaMenu: evt.
	self addTitleForHaloMenu: menu.
	menu popUpEvent: evt in: self world
]

{ #category : 'classification' }
Morph >> isAlignmentMorph [

	^ false
]

{ #category : 'testing' }
Morph >> isDockingBar [
	"Return true if the receiver is a docking bar"
	^ false
]

{ #category : 'classification' }
Morph >> isFlexMorph [

	^ false
]

{ #category : 'testing' }
Morph >> isFlexed [
	"Return true if the receiver is currently flexed"
	^ owner isNotNil and: [owner isFlexMorph]
]

{ #category : 'testing' }
Morph >> isFullOnScreen [
	"Answer if the receiver is full contained in the owner visible
	area."
	owner ifNil: [^ true].
	self visible ifFalse: [^ true].
	^ owner clearArea containsRect: self fullBounds
]

{ #category : 'testing' }
Morph >> isHaloMorph [

	^ false
]

{ #category : 'classification' }
Morph >> isHandMorph [

	^ false
]

{ #category : 'structure' }
Morph >> isInDockingBar [
	"answer if the receiver is in a menu bar"
	^ (owner isNotNil) and: [owner isDockingBar]
]

{ #category : 'structure' }
Morph >> isInWorld [
	"Return true if this morph is in a world."

	^self world isNotNil
]

{ #category : 'testing' }
Morph >> isLineMorph [
	^false
]

{ #category : 'accessing' }
Morph >> isLocked [
	"answer whether the receiver is Locked"

	^ extension ifNil: [ false ] ifNotNil: [ :ext | ext locked ]
]

{ #category : 'testing' }
Morph >> isMorph [

	^ true
]

{ #category : 'classification' }
Morph >> isRenderer [
	"A *renderer* morph transforms the appearance of its submorph in some manner. For example, it might supply a drop shadow or scale and rotate the morph it encases. Answer true if this morph acts as a renderer. This default implementation returns false."
	"Details: A renderer is assumed to have a single submorph. Renderers may be nested to concatenate their transformations. It is useful to be able to find the outer-most renderer. This can be done by ascending the owner chain from the rendered morph. To find the morph being rendered, one can descend through the (singleton) submorph lists of the renderer chain until a non-renderer is encountered."

	^ false
]

{ #category : 'stepping and presenter' }
Morph >> isStepping [
	"Return true if the receiver is currently stepping in its world"
	| aWorld |
	^ (aWorld := self world)
		ifNil:		[false]
		ifNotNil:	[aWorld isStepping: self]
]

{ #category : 'stepping and presenter' }
Morph >> isSteppingSelector: aSelector [
	"Return true if the receiver is currently stepping in its world"
	| aWorld |
	^ (aWorld := self world)
		ifNil:		[false]
		ifNotNil:	[aWorld isStepping: self selector: aSelector]
]

{ #category : 'testing' }
Morph >> isSticky [
	"answer whether the receiver is Sticky"

	^ extension ifNil: [ false ] ifNotNil: [ :ext | ext sticky ]
]

{ #category : 'classification' }
Morph >> isTextMorph [
	^false
]

{ #category : 'testing' }
Morph >> isTranslucentButNotTransparent [
	"Answer true if this any of this morph is translucent but not transparent."

	^ color isColor and: [ color isTranslucentButNotTransparent ]
]

{ #category : 'classification' }
Morph >> isWorldMorph [

	^ false
]

{ #category : 'classification' }
Morph >> isWorldOrHandMorph [

	^ self isWorldMorph or: [self isHandMorph]
]

{ #category : 'dropping/grabbing' }
Morph >> justGrabbedFrom: formerOwner [
	"The receiver was just grabbed from its former owner and is now attached to the hand. By default, we pass this message on if we're a renderer."
	(self isRenderer and:[self hasSubmorphs])
		ifTrue:[self firstSubmorph justGrabbedFrom: formerOwner]
]

{ #category : 'rotate scale and flex' }
Morph >> keepsTransform [
	"Return true if the receiver will keep it's transform while being grabbed by a hand."
	^false
]

{ #category : 'event handling' }
Morph >> keyDown: anEvent [
	"Handle a key down event. The default response is to do nothing."
	^ false
]

{ #category : 'event handling' }
Morph >> keyStroke: anEvent [
	"Handle a keystroke event.  The default response is to let my eventHandler, if any, handle it."
	^false
]

{ #category : 'event handling' }
Morph >> keyUp: anEvent [
	"Handle a key up event. The default response is to do nothing."
	^ false
]

{ #category : 'event handling' }
Morph >> keyboardFocusChange: gotFocus [
	"The message is sent to a morph when its keyboard focus change. The given argument indicates that the receiver is gaining keyboard focus (versus losing) the keyboard focus. Morphs that accept keystrokes should change their appearance in some way when they are the current keyboard focus. This default implementation does nothing."
	self announceKeyboardFocusChange: gotFocus
]

{ #category : 'submorphs - accessing' }
Morph >> lastSubmorph [
	^submorphs last
]

{ #category : 'layout' }
Morph >> layoutBounds [
	"Return the bounds for laying out children of the receiver"
	| inset box |
	inset := self layoutInset.
	box := self innerBounds.
	inset isZero ifTrue:[^box].
	^box insetBy: inset
]

{ #category : 'layout' }
Morph >> layoutBounds: aRectangle [
	"Set the bounds for laying out children of the receiver.
	Note: written so that #layoutBounds can be changed without touching this method"
	| outer inner |
	outer := self bounds.
	inner := self layoutBounds.
	bounds := aRectangle origin + (outer origin - inner origin) corner:
				aRectangle corner + (outer corner - inner corner)
]

{ #category : 'recategorized' }
Morph >> layoutChanged [
	"Fixed to always flush layout cache - finally tracked down
	layout anomalies due to cached extents in layout
	policies not being flushed, the previous (incorrect) assumption being
	that it did not matter if layout was to be recomputed (fullBounds being nil).
	Recomputing of the layout apparently does not flush so must be done here."

	| layout |
	fullBounds := nil.
	layout := self layoutPolicy.
	layout ifNotNil:[layout flushLayoutCache].
	owner ifNotNil: [owner layoutChanged].
	"note: does not send #ownerChanged here - we'll do this when computing the new layout"
]

{ #category : 'layout - properties' }
Morph >> layoutFrame [
	"Layout specific. Return the layout frame describing where the
	receiver should appear in a proportional layout"
	^ extension ifNotNil: [extension layoutFrame]
]

{ #category : 'layout - properties' }
Morph >> layoutFrame: aLayoutFrame [
	"Layout specific. Return the layout frame describing where the receiver should appear in a proportional layout"
	self layoutFrame == aLayoutFrame ifTrue: [^self].
	self assureExtension layoutFrame: aLayoutFrame asLayoutFrame.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> layoutInset [
	"Return the extra inset for layouts"

	^ self layoutProperties ifNil: [ 0 ] ifNotNil: [ :props | props layoutInset ]
]

{ #category : 'layout - properties' }
Morph >> layoutInset: aNumber [
	"Return the extra inset for layouts"
	self assureTableProperties layoutInset: aNumber.
	self layoutChanged
]

{ #category : 'layout - menu' }
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

{ #category : 'layout - properties' }
Morph >> layoutPolicy [
	"Layout specific. Return the layout policy describing how children
	of the receiver should appear."
	^ extension ifNotNil: [ extension layoutPolicy]
]

{ #category : 'layout - properties' }
Morph >> layoutPolicy: aLayoutPolicy [
	"Layout specific. Return the layout policy describing how children of the receiver should appear."
	self layoutPolicy == aLayoutPolicy ifTrue:[^self].
	self assureExtension layoutPolicy: aLayoutPolicy.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> layoutProperties [
	"Return the current layout properties associated with the
	receiver"
	^ extension ifNotNil: [ extension layoutProperties]
]

{ #category : 'layout - properties' }
Morph >> layoutProperties: newProperties [
	"Return the current layout properties associated with the receiver"
	self layoutProperties == newProperties ifTrue:[^self].
	self assureExtension layoutProperties: newProperties
]

{ #category : 'layout' }
Morph >> layoutProportionallyIn: newBounds [
	"Layout specific. Apply the given bounds to the receiver."
	| box frame |
	frame := self layoutFrame ifNil:[^self].
	"before applying the proportional values make sure the receiver's layout is computed"
	"compute the cell size the receiver has given its layout frame"
	box := frame layout: self bounds in: newBounds.
	(box = self bounds) ifTrue:[^self]. "no change"
	^self layoutInBounds: box
]

{ #category : 'geometry' }
Morph >> left [
	" Return the x-coordinate of my left side "

	^ bounds left
]

{ #category : 'geometry' }
Morph >> left: aNumber [
	" Move me so that my left side is at the x-coordinate aNumber. My extent (width & height) are unchanged "

	self position: (aNumber @ bounds top)
]

{ #category : 'geometry' }
Morph >> leftCenter [

	^ bounds leftCenter
]

{ #category : 'layout - properties' }
Morph >> listCentering [
	"Layout specific. This property describes how the rows/columns in a list-like layout should be centered.
		#topLeft - center at start of primary direction
		#bottomRight - center at end of primary direction
		#center - center in the middle of primary direction
		#justified - insert extra space inbetween rows/columns
	"
	| props |
	props := self layoutProperties.
	^props ifNil:[#topLeft] ifNotNil:[props listCentering]
]

{ #category : 'layout - properties' }
Morph >> listCentering: aSymbol [
	"Layout specific. This property describes how the rows/columns in a list-like layout should be centered.
		#topLeft - center at start of primary direction
		#bottomRight - center at end of primary direction
		#center - center in the middle of primary direction
		#justified - insert extra space inbetween rows/columns
	"
	self assureTableProperties listCentering: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> listCenteringString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self listCentering
]

{ #category : 'layout - properties' }
Morph >> listDirection [
	"Layout specific. This property describes the direction in which a list-like layout should be applied. Possible values are:
		#leftToRight
		#rightToLeft
		#topToBottom
		#bottomToTop
	indicating the direction in which any layout should take place"
	| props |
	props := self layoutProperties.
	^props ifNil:[#topToBottom] ifNotNil:[props listDirection]
]

{ #category : 'layout - properties' }
Morph >> listDirection: aSymbol [
	"Layout specific. This property describes the direction in which a list-like layout should be applied. Possible values are:
		#leftToRight
		#rightToLeft
		#topToBottom
		#bottomToTop
	indicating the direction in which any layout should take place"
	self assureTableProperties listDirection: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> listDirectionString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self listDirection
]

{ #category : 'base - widgets' }
Morph >> listRenderOn: aCanvas atRow: aRow bounds: drawBounds color: drawColor backgroundColor: backgroundColor from: aMorph [

	self bounds: drawBounds.
	self fullDrawOn: aCanvas.
	(aMorph submorphs includes: self)
		ifFalse: [ aMorph addMorph: self ]
]

{ #category : 'layout - properties' }
Morph >> listSpacing [
	"Layout specific. This property describes how the heights for different rows in a table layout should be handled.
		#equal - all rows have the same height
		#none - all rows may have different heights
	"
	^ self layoutProperties
		  ifNil: [ #none ]
		  ifNotNil: [ :props | props listSpacing ]
]

{ #category : 'layout - properties' }
Morph >> listSpacing: aSymbol [
	"Layout specific. This property describes how the heights for different rows in a table layout should be handled.
		#equal - all rows have the same height
		#none - all rows may have different heights
	"
	self assureTableProperties listSpacing: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> listSpacingString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self listSpacing
]

{ #category : 'geometry' }
Morph >> localPointToGlobal: aPoint [
	^self point: aPoint in: nil
]

{ #category : 'accessing' }
Morph >> lock [
	self lock: true
]

{ #category : 'accessing' }
Morph >> lock: aBoolean [
	"change the receiver's lock property"
	(extension isNil and: [aBoolean not]) ifTrue: [^ self].
	self assureExtension locked: aBoolean
]

{ #category : 'menus' }
Morph >> lockUnlockMorph [
	"If the receiver is locked, unlock it; if unlocked, lock it"

	self isLocked ifTrue: [self unlock] ifFalse: [self lock]
]

{ #category : 'menus' }
Morph >> lockedString [
	"Answer the string to be shown in a menu to represent the 'locked' status"
	^ (self isLocked) -> 'be locked' translated
]

{ #category : 'submorphs - accessing' }
Morph >> mainDockingBars [
	"Answer the receiver's main dockingBars"
	^ self dockingBars
		select: [:each | each hasProperty: #mainDockingBarTimeStamp]
]

{ #category : 'layout - properties' }
Morph >> maxCellSize [
	"Layout specific. This property specifies the maximum size of a table cell."
	| props |
	props := self layoutProperties.
	^props ifNil:[SmallInteger maxVal] ifNotNil:[props maxCellSize]
]

{ #category : 'layout - properties' }
Morph >> maxCellSize: aPoint [
	"Layout specific. This property specifies the maximum size of a table cell."
	self assureTableProperties maxCellSize: aPoint.
	self layoutChanged
]

{ #category : 'menus' }
Morph >> maybeAddCollapseItemTo: aMenu [
	"If appropriate, add a collapse item to the given menu"

	| anOwner |
	(anOwner := self topRendererOrSelf owner) ifNotNil:
			[anOwner isWorldMorph ifTrue:
				[aMenu add: 'collapse' target: self selector: #collapse]]
]

{ #category : 'meta-actions' }
Morph >> maybeDuplicateMorph [
	"Maybe duplicate the morph"

	^self duplicate openInHand
]

{ #category : 'meta-actions' }
Morph >> maybeDuplicateMorph: evt [
	^self duplicateMorph: evt
]

{ #category : 'settings' }
Morph >> menuKeyboardControl [
	^ self theme settings menuKeyboardControl
]

{ #category : 'layout - properties' }
Morph >> minCellSize [
	"Layout specific. This property specifies the minimal size of a table cell."
	| props |
	props := self layoutProperties.
	^props ifNil:[0] ifNotNil:[props minCellSize]
]

{ #category : 'layout - properties' }
Morph >> minCellSize: aPoint [
	"Layout specific. This property specifies the minimal size of a table cell."
	self assureTableProperties minCellSize: aPoint.
	self layoutChanged
]

{ #category : 'layout' }
Morph >> minHeight [
	"answer the receiver's minHeight"
	^ self
		valueOfProperty: #minHeight
		ifAbsent: [2]
]

{ #category : 'layout' }
Morph >> minHeight: aNumber [
	aNumber
		ifNil: [self removeProperty: #minHeight]
		ifNotNil: [self setProperty: #minHeight toValue: aNumber].
	self layoutChanged
]

{ #category : 'layout' }
Morph >> minWidth [
	"answer the receiver's minWidth"
	^ self
		valueOfProperty: #minWidth
		ifAbsent: [2]
]

{ #category : 'layout' }
Morph >> minWidth: aNumber [
	aNumber
		ifNil: [self removeProperty: #minWidth]
		ifNotNil: [self setProperty: #minWidth toValue: aNumber].
	self layoutChanged
]

{ #category : 'geometry' }
Morph >> minimumExtent [
	| ext |
	"This returns the minimum extent that the morph may be shrunk to.  Not honored in too many places yet, but respected by the resizeToFit feature, at least.  copied up from SystemWindow 6/00"
	(ext := self valueOfProperty: #minimumExtent)
		ifNotNil:
			[^ ext].
	^ 100 @ 80
]

{ #category : 'geometry' }
Morph >> minimumExtent: aPoint [
	"Remember a minimumExtent, for possible future use"

	self setProperty: #minimumExtent toValue: aPoint
]

{ #category : 'menus' }
Morph >> model [
	^ nil
]

{ #category : 'structure' }
Morph >> morphPreceding: aSubmorph [
	"Answer the morph immediately preceding aSubmorph, or nil if none"

	| anIndex |
	anIndex := submorphs indexOf: aSubmorph ifAbsent: [^ nil].
	^ anIndex > 1
		ifTrue:
			[submorphs at: (anIndex - 1)]
		ifFalse:
			[nil]
]

{ #category : 'wiw support' }
Morph >> morphicLayerNumber [

	"helpful for insuring some morphs always appear in front of or behind others.
	smaller numbers are in front"

	^(owner isNil or: [owner isWorldMorph]) ifTrue: [
		self valueOfProperty: #morphicLayerNumber ifAbsent: [100]
	] ifFalse: [
		owner morphicLayerNumber
	].

	"leave lots of room for special things"
]

{ #category : 'submorphs - accessing' }
Morph >> morphsAt: aPoint [
	"Return a collection of all morphs in this morph structure that contain the given point, possibly including the receiver itself.  The order is deepest embedding first."
	^self morphsAt: aPoint unlocked: false
]

{ #category : 'submorphs - accessing' }
Morph >> morphsAt: aPoint behind: aMorph unlocked: aBool [
	"Return all morphs at aPoint that are behind frontMorph; if aBool is true return only unlocked, visible morphs."

	| isBack all tfm |
	all := (aMorph isNil or: [owner isNil])
				ifTrue:
					["Traverse down"

					(self fullBounds containsPoint: aPoint) ifFalse: [^#()].
					(aBool and: [self isLocked or: [self visible not]]) ifTrue: [^#()].
					nil]
				ifFalse:
					["Traverse up"

					tfm := self transformedFrom: owner.
					all := owner
								morphsAt: (tfm localPointToGlobal: aPoint)
								behind: self
								unlocked: aBool.
					WriteStream with: all].
	isBack := aMorph isNil.
	self submorphsDo:
			[:m | | found |
			isBack
				ifTrue:
					[tfm := m transformedFrom: self.
					found := m
								morphsAt: (tfm globalPointToLocal: aPoint)
								behind: nil
								unlocked: aBool.
					found notEmpty
						ifTrue:
							[all ifNil: [all := Array new  writeStream].
							all nextPutAll: found]].
			m == aMorph ifTrue: [isBack := true]].
	(isBack and: [self containsPoint: aPoint])
		ifTrue:
			[all ifNil: [^Array with: self].
			all nextPut: self].
	^all ifNil: [#()] ifNotNil: [all contents]
]

{ #category : 'submorphs - accessing' }
Morph >> morphsAt: aPoint unlocked: aBool [
	"Return a collection of all morphs in this morph structure that contain the given point, possibly including the receiver itself.  The order is deepest embedding first."
	| mList |
	mList := Array new writeStream.
	self morphsAt: aPoint unlocked: aBool do:[:m| mList nextPut: m].
	^mList contents
]

{ #category : 'submorphs - accessing' }
Morph >> morphsAt: aPoint unlocked: aBool do: aBlock [
	"Evaluate aBlock with all the morphs starting at the receiver which appear at aPoint. If aBool is true take only visible, unlocked morphs into account."

	(self fullBounds containsPoint: aPoint) ifFalse:[^self].
	(aBool and:[self isLocked or:[self visible not]]) ifTrue:[^self].
	self submorphsDo:[:m| | tfm |
		tfm := m transformedFrom: self.
		m morphsAt: (tfm globalPointToLocal: aPoint) unlocked: aBool do: aBlock].
	(self containsPoint: aPoint) ifTrue:[aBlock value: self]
]

{ #category : 'submorphs - accessing' }
Morph >> morphsInFrontOf: someMorph overlapping: aRectangle do: aBlock [
	"Evaluate aBlock with all top-level morphs in front of someMorph that overlap with the given rectangle. someMorph is either an immediate child of the receiver or nil (in which case all submorphs of the receiver are enumerated)."
	self submorphsDo:[:m|
		m == someMorph ifTrue:["Try getting out quickly"
			owner ifNil:[^self].
			^owner morphsInFrontOf: self overlapping: aRectangle do: aBlock].
		(m fullBoundsInWorld intersects: aRectangle)
			ifTrue:[aBlock value: m]].
	owner ifNil:[^self].
	^owner morphsInFrontOf: self overlapping: aRectangle do: aBlock
]

{ #category : 'submorphs - accessing' }
Morph >> morphsInFrontOverlapping: aRectangle [
	"Return all top-level morphs in front of someMorph that overlap with the given rectangle."
	| morphList |
	morphList := Array new writeStream.
	self morphsInFrontOf: nil overlapping: aRectangle do:[:m | morphList nextPut: m].
	^morphList contents
]

{ #category : 'submorphs - accessing' }
Morph >> morphsInFrontOverlapping: aRectangle do: aBlock [
	"Evaluate aBlock with all top-level morphs in front of someMorph that overlap with the given rectangle."
	^self morphsInFrontOf: nil overlapping: aRectangle do: aBlock
]

{ #category : 'event handling' }
Morph >> mouseDown: evt [
	"Handle a mouse down event. It can be handled here or in the event handler"
]

{ #category : 'halos and balloon help' }
Morph >> mouseDownOnHelpHandle: anEvent [
	"The mouse went down in the show-balloon handle"

	| str |
	anEvent shiftPressed ifTrue: [^ self editBalloonHelpText].
	str := self balloonText.
	str ifNil: [str := self noHelpString].
	self showBalloon: str hand: anEvent hand
]

{ #category : 'events - processing' }
Morph >> mouseDownPriority [
	"Return the default mouse down priority for the receiver"

	^ 0
]

{ #category : 'event handling' }
Morph >> mouseEnter: evt [
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed"
]

{ #category : 'event handling' }
Morph >> mouseEnterDragging: evt [
	"Handle a mouseEnterDragging event, meaning the mouse just entered my bounds with a button pressed or laden with submorphs.  The default response is to let my eventHandler, if any, handle it, or else to do nothing."

	self eventHandler ifNotNil:
		[^ self eventHandler mouseEnterDragging: evt fromMorph: self]
]

{ #category : 'event handling' }
Morph >> mouseLeave: evt [
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."
]

{ #category : 'event handling' }
Morph >> mouseLeaveDragging: evt [
	"Handle a mouseLeaveLaden event, meaning the mouse just left my bounds with a button pressed or laden with submorphs."
]

{ #category : 'event handling' }
Morph >> mouseMove: evt [
	"Handle a mouse move event. The default response is to let my eventHandler, if any, handle it."
]

{ #category : 'event handling' }
Morph >> mouseStillDown: evt [
	"Handle a mouse move event. The default response is to let my eventHandler, if any, handle it."
]

{ #category : 'event handling' }
Morph >> mouseStillDownStepRate [
	"At what rate do I want to receive #mouseStillDown: notifications?"
	^1
]

{ #category : 'event handling' }
Morph >> mouseStillDownThreshold [
	"Return the number of milliseconds after which mouseStillDown: should be sent"
	^200
]

{ #category : 'event handling' }
Morph >> mouseUp: evt [
	"Handle a mouse up event. The default response is to let my eventHandler, if any, handle it."
]

{ #category : 'event handling' }
Morph >> mouseWheel: evt [
	"Handle a mouseWheel event."
]

{ #category : 'event handling' }
Morph >> moveOrResizeFromKeystroke: anEvent [
	"move or resize the receiver based on a keystroke"

	| dir |
	anEvent keyValue = 28 ifTrue: [ dir := -1 @ 0 ].
	anEvent keyValue = 29 ifTrue: [ dir := 1 @ 0 ].
	anEvent keyValue = 30 ifTrue: [ dir := 0 @ -1 ].
	anEvent keyValue = 31 ifTrue: [ dir := 0 @ 1 ].
	dir ifNotNil: [
		anEvent controlKeyPressed ifTrue: [ dir := dir * 10 ].
		anEvent shiftPressed
			ifTrue: [ self extent: self extent + dir ]
			ifFalse: [ self position: self position + dir ] ]
]

{ #category : 'naming' }
Morph >> name: aName [
	(aName isString) ifTrue: [self setNameTo: aName]
]

{ #category : 'structure' }
Morph >> nearestOwnerThat: conditionBlock [
	"Return the first enclosing morph for which aBlock evaluates to true, or nil if none"

	^ self firstOwnerSuchThat: conditionBlock
]

{ #category : 'halos and balloon help' }
Morph >> noHelpString [
	^ 'Help not yet supplied' translated
]

{ #category : 'submorphs - accessing' }
Morph >> noteNewOwner: aMorph [
	"I have just been added as a submorph of aMorph"
]

{ #category : 'halos and balloon help' }
Morph >> okayToAddDismissHandle [
	"Answer whether a halo on the receiver should offer a dismiss handle.  This provides a hook for making it harder to disassemble some strucures even momentarily"

	^self resistsRemoval not
]

{ #category : 'halos and balloon help' }
Morph >> okayToBrownDragEasily [
	"Answer whether it it okay for the receiver to be brown-dragged easily -- i.e. repositioned within its container without extracting it.  At present this is just a hook -- nobody declines."

	^ true



"
	^ (self topRendererOrSelf owner isKindOf: PasteUpMorph) and:
		[self layoutPolicy isNil]"
]

{ #category : 'halos and balloon help' }
Morph >> okayToResizeEasily [
	"Answer whether it is appropriate to have the receiver be easily resized by the user from the halo"

	^ true

	"This one was too jarring, not that it didn't most of the time do the right  thing but because some of the time it didn't, such as in a holder.  If we pursue this path, the test needs to be airtight, obviously...
	^ (self topRendererOrSelf owner isKindOf: PasteUpMorph) and:
		[self layoutPolicy isNil]"
]

{ #category : 'halos and balloon help' }
Morph >> okayToRotateEasily [
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver.  This is a hook -- at present nobody declines."

	^ self respondsTo: #prepareForRotating
]

{ #category : 'convenience' }
Morph >> on: aShortcut do: anAction [

	self
		deprecated: 'Use bindKeyCombination:toAction:'
		on: '16/11/2024'
		in: #Pharo13
		transformWith: '`@receiver on: `@aShortcut do: `@anAction'
						-> '`@receiver bindKeyCombination: `@aShortcut toAction: `@anAction'.
						
	^ self bindKeyCombination: aShortcut toAction: anAction
	


]

{ #category : 'announcements' }
Morph >> onAnnouncement: anAnnouncement do: aValuable [

	self announcer
		when: anAnnouncement
		do: aValuable
		for: aValuable receiver
]

{ #category : 'announcements' }
Morph >> onAnnouncement: anAnnouncement send: aMessageSelector to: anObject [
	self announcer when: anAnnouncement send: aMessageSelector to: anObject
]

{ #category : 'showing' }
Morph >> openCenteredInWorld [
	"Open me in the center of current world"
	self
		fullBounds;
		openInWorld;
		center: self world clearArea center
]

{ #category : 'showing' }
Morph >> openInHand [
	"Attach the receiver to the current hand in the current morphic world"

	self currentHand attachMorph: self
]

{ #category : 'opening' }
Morph >> openInWorld [
	self openInWorld: self currentWorld
]

{ #category : 'opening' }
Morph >> openInWorld: aWorld [
	self preOpenInWorld: aWorld.
	self doOpenInWorld: aWorld.
	self postOpenInWorld: aWorld.
	self announceOpened
]

{ #category : 'accessing - properties' }
Morph >> otherProperties [
	"answer the receiver's otherProperties"
	^ extension ifNotNil: [extension otherProperties]
]

{ #category : 'initialize' }
Morph >> outOfWorld: aWorld [
	"The receiver has just appeared in a new world. Notes:
		* aWorld can be nil (due to optimizations in other places)
		* owner is still valid
	Important: Keep this method fast - it is run whenever morphs are removed."
	aWorld ifNil:[^self].
	"ar 1/31/2001: We could explicitly stop stepping the receiver here but for the sake of speed I'm for now relying on the lazy machinery in the world itself."
	"aWorld stopStepping: self."
	self submorphsDo:[:m| m outOfWorld: aWorld]
]

{ #category : 'geometry' }
Morph >> outerBounds [
	"Return the 'outer' bounds of the receiver, e.g., the bounds that need to be invalidated when the receiver changes."
	| box |
	box := self bounds.
	self hasDropShadow ifTrue:[box := self expandFullBoundsForDropShadow: box].
	^box
]

{ #category : 'structure' }
Morph >> outermostMorphThat: conditionBlock [
	"Return the outermost containing morph for which aBlock is true, or nil if none"

	| outermost |
	self allOwnersDo: [:m | (conditionBlock value: m) ifTrue: [outermost := m]].
	^ outermost
]

{ #category : 'menu' }
Morph >> outermostOwnerWithYellowButtonMenu [
	"Answer me or my outermost owner that is willing to contribute menu items to a context menu.
	Don't include the world."

	| outermost |
	outermost := self outermostMorphThat: [ :ea |
		ea isWorldMorph not and: [ ea hasYellowButtonMenu ]].
	^outermost ifNil: [ self hasYellowButtonMenu ifTrue: [ self ] ifFalse: []]
]

{ #category : 'structure' }
Morph >> outermostWorldMorph [

	^self world ifNil: [ self currentWorld ]
]

{ #category : 'structure' }
Morph >> owner [
	"Returns the owner of this morph, which may be nil."

	^ owner
]

{ #category : 'debug and other' }
Morph >> ownerChain [
	"Answer a list of objects representing the receiver and all of its owners.   The first element is the receiver, and the last one is typically the world in which the receiver resides"

	| c next |
	c := OrderedCollection with: self.
	next := self.
	[(next := next owner) isNotNil] whileTrue: [c add: next].
	^c asArray
]

{ #category : 'change reporting' }
Morph >> ownerChanged [
	"The receiver's owner, some kind of a pasteup, has changed its layout."

	self snapToEdgeIfAppropriate
]

{ #category : 'structure' }
Morph >> ownerThatIsA: aClass [
	"Return the first enclosing morph that is a kind of aClass, or nil if none"

	^ self firstOwnerSuchThat: [:m | m isKindOf: aClass]
]

{ #category : 'base - worlds' }
Morph >> pasteUpMorph [
	"Answer the closest containing morph that is a PasteUp morph"
	^ self ownerThatIsA: PasteUpMorph
]

{ #category : 'base - worlds' }
Morph >> pasteUpMorphHandlingTabAmongFields [
	"Answer the nearest PasteUpMorph in my owner chain that has the tabAmongFields property, or nil if none"

	| aPasteUp |
	aPasteUp := self owner.
	[aPasteUp isNotNil] whileTrue:
		[aPasteUp tabAmongFields ifTrue:
			[^ aPasteUp].
		aPasteUp := aPasteUp owner].
	^ nil
]

{ #category : 'thumbnail' }
Morph >> permitsThumbnailing [
	^ true
]

{ #category : 'geometry' }
Morph >> point: aPoint from: aReferenceMorph [

	owner ifNil: [^ aPoint].
	^ (owner transformFrom: aReferenceMorph) globalPointToLocal: aPoint
]

{ #category : 'geometry' }
Morph >> point: aPoint in: aReferenceMorph [

	owner ifNil: [^ aPoint].
	^ (owner transformFrom: aReferenceMorph) localPointToGlobal: aPoint
]

{ #category : 'geometry' }
Morph >> pointFromWorld: aPoint [
	^self point: aPoint from: self world
]

{ #category : 'geometry' }
Morph >> pointInWorld: aPoint [
	^self point: aPoint in: self world
]

{ #category : 'geometry' }
Morph >> position [

	^ bounds topLeft
]

{ #category : 'geometry' }
Morph >> position: aPoint [
	"Change the position of this morph and and all of its
	submorphs. "
	| delta box |
	delta := aPoint asNonFractionalPoint - bounds topLeft.
	(delta x = 0 and: [delta y = 0])
		ifTrue: [^ self].
	"Null change"
	box := self fullBounds.
	(delta dotProduct: delta) > 100
		ifTrue: ["e.g., more than 10 pixels moved"
			self invalidRect: box.
			self invalidRect: (box translateBy: delta)]
		ifFalse: [self invalidRect: (box merge: (box translateBy: delta))].
	self privateFullMoveBy: delta.
	owner ifNotNil: [owner layoutChanged]
]

{ #category : 'geometry' }
Morph >> positionInWorld [

	^ self pointInWorld: self position
]

{ #category : 'geometry' }
Morph >> positionSubmorphs [
	self submorphsDo:
		[:aMorph | aMorph snapToEdgeIfAppropriate]
]

{ #category : 'opening' }
Morph >> postOpenInWorld: aWorld [

	self startSteppingSubmorphs
]

{ #category : 'meta-actions' }
Morph >> potentialEmbeddingTargets [
	"Return the potential targets for embedding the receiver"

	| oneUp topRend |
	(oneUp := (topRend := self topRendererOrSelf) owner) ifNil: [ ^ #() ].
	^ (oneUp morphsAt: topRend referencePosition behind: topRend unlocked: true) reject: [ :m | m isFlexMorph ]
]

{ #category : 'meta-actions' }
Morph >> potentialTargets [
	"Return the potential targets for the receiver.
	This is derived from Morph>>potentialEmbeddingTargets."
	owner ifNil:[^#()].
	^owner morphsAt: self referencePosition behind: self unlocked: true not
]

{ #category : 'meta-actions' }
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

{ #category : 'opening' }
Morph >> preOpenInWorld: aWorld [
	"Interact with a world to decide how to position myself.
	By default, I choose the visible area of the world.
	Subclasses may choose different strategies or even calculate their bounds.
	"
	(aWorld visibleClearArea origin ~= (0@0) and: [self position = (0@0)]) ifTrue:
		[self position: aWorld visibleClearArea origin]
]

{ #category : 'halos and balloon help' }
Morph >> preferredDuplicationHandleSelector [
	"Answer the selector, either #addMakeSiblingHandle: or addDupHandle:, to be offered as the default in a halo open on me"

	^ #addDupHandle:
]

{ #category : 'event handling' }
Morph >> preferredKeyboardBounds [

	^ self bounds: self bounds in: self world
]

{ #category : 'event handling' }
Morph >> preferredKeyboardPosition [

	^ self preferredKeyboardBounds topLeft
]

{ #category : 'rotate scale and flex' }
Morph >> prepareForScaling [
	"If this morph requires a flex shell to scale,
	then wrap it in one and return it.
	Polygons, eg, may override to return themselves."

	^ self addFlexShell
]

{ #category : 'copying' }
Morph >> prepareToBeSaved [
	"Prepare this morph to be saved to disk. Subclasses should nil out any instance variables that holds state that should not be saved, such as cached Forms. Note that this operation may take more drastic measures than releaseCachedState; for example, it might discard the transcript of an interactive chat session."

	self releaseCachedState.
	self formerOwner: nil.
	self formerPosition: nil.
	fullBounds := nil
]

{ #category : 'menus' }
Morph >> presentHelp [
	"Present a help message if there is one available"

	self inform: 'Sorry, no help has been
provided here yet.'
]

{ #category : 'structure' }
Morph >> primaryHand [

        | outer |
        outer := self outermostWorldMorph ifNil: [^ nil].
        ^ outer activeHand ifNil: [outer firstHand]
]

{ #category : 'printing' }
Morph >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: '('.
	aStream
		print: self identityHash;
		nextPutAll: ')'
]

{ #category : 'printing' }
Morph >> printStructureOn: aStream indent: tabCount [

	tabCount timesRepeat: [aStream tab].
	self printOn: aStream.
	aStream cr.
	self submorphsDo: [:m | m printStructureOn: aStream indent: tabCount + 1]
]

{ #category : 'private' }
Morph >> privateAddAllMorphs: aCollection atIndex: index [
	"Private. Add aCollection of morphs to the receiver"
	| myWorld otherSubmorphs |
	myWorld := self world.
	otherSubmorphs := submorphs copyWithoutAll: aCollection.
	(index between: 0 and: otherSubmorphs size)
		ifFalse: [^ self error: 'index out of range'].
	index = 0
		ifTrue:[	submorphs := aCollection asArray, otherSubmorphs]
		ifFalse:[	index = otherSubmorphs size
			ifTrue:[	submorphs := otherSubmorphs, aCollection]
			ifFalse:[	submorphs := otherSubmorphs copyReplaceFrom: index + 1 to: index with: aCollection ]].
	aCollection do: [:m | | itsOwner itsWorld |
		itsOwner := m owner.
		itsOwner ifNotNil: [
			itsWorld := m world.
			(itsWorld == myWorld) ifFalse: [
				itsWorld ifNotNil: [self privateInvalidateMorph: m].
				m outOfWorld: itsWorld].
			(itsOwner ~~ self) ifTrue: [
				m owner privateRemove: m.
				m owner removedMorph: m ]].
		m privateOwner: self.
		myWorld ifNotNil: [self privateInvalidateMorph: m].
		(myWorld == itsWorld) ifFalse: [m intoWorld: myWorld].
		itsOwner == self ifFalse: [
			self addedMorph: m.
			m noteNewOwner: self ].
	].
	self layoutChanged
]

{ #category : 'private' }
Morph >> privateAddMorph: aMorph atIndex: index [
	| oldIndex myWorld itsWorld oldOwner |
	(index between: 1 and: submorphs size + 1)
		ifFalse: [ ^ self error: 'index out of range' ].
	myWorld := self world.
	oldOwner := aMorph owner.
	(oldOwner == self and: [ (oldIndex := submorphs indexOf: aMorph) > 0 ])
		ifTrue: [
			"aMorph's position changes within in the submorph chain"
			oldIndex < index
				ifTrue: [
					"moving aMorph to back"
					submorphs
						replaceFrom: oldIndex
						to: index - 2
						with: submorphs
						startingAt: oldIndex + 1.
					submorphs at: index - 1 put: aMorph ]
				ifFalse: [
					"moving aMorph to front"
					oldIndex - 1 to: index by: -1 do: [ :i | submorphs at: i + 1 put: (submorphs at: i) ].
					submorphs at: index put: aMorph ] ]
		ifFalse: [
			"adding a new morph"
			oldOwner
				ifNotNil: [
					itsWorld := aMorph world.
					itsWorld ifNotNil: [ self privateInvalidateMorph: aMorph ].
					itsWorld == myWorld
						ifFalse: [ aMorph outOfWorld: itsWorld ].
					oldOwner privateRemove: aMorph.
					oldOwner removedMorph: aMorph ].
			aMorph privateOwner: self.
			submorphs := submorphs copyReplaceFrom: index to: index - 1 with: { aMorph } .
			itsWorld == myWorld
				ifFalse: [ aMorph intoWorld: myWorld ] ].
	myWorld ifNotNil: [ self privateInvalidateMorph: aMorph ].
	self layoutChanged.
	oldOwner == self
		ifFalse: [
			self addedMorph: aMorph.
			aMorph noteNewOwner: self ]
]

{ #category : 'private' }
Morph >> privateBounds: boundsRect [
	"Private! Use position: and/or extent: instead."

	fullBounds := nil.
	bounds := boundsRect
]

{ #category : 'private' }
Morph >> privateColor: aColor [

	color := aColor
]

{ #category : 'submorphs - add/remove' }
Morph >> privateDelete [
	"Remove the receiver as a submorph of its owner"
	owner ifNotNil:[owner removeMorph: self]
]

{ #category : 'accessing - extension' }
Morph >> privateExtension: aMorphExtension [
	"private - change the receiver's extension"
	extension := aMorphExtension
]

{ #category : 'layout' }
Morph >> privateFullBounds [
	"Private. Compute the actual full bounds of the receiver"

	| box |
	submorphs isEmpty ifTrue: [^self outerBounds].
	box := self outerBounds copy.
	box := box quickMerge: (self clipSubmorphs
						ifTrue: [self submorphBounds intersect: self clippingBounds ifNone: [ self clippingBounds ]]
						ifFalse: [self submorphBounds]).
	^box origin asIntegerPoint corner: box corner asIntegerPoint
]

{ #category : 'private' }
Morph >> privateFullBounds: boundsRect [
	"Private! Computed automatically."

	fullBounds := boundsRect
]

{ #category : 'private' }
Morph >> privateFullMoveBy: delta [
	"Private! Relocate me and all of my subMorphs by recursion. Subclasses that implement different coordinate systems may override this method."

	self privateMoveBy: delta.
	submorphs do: [:each | each privateFullMoveBy: delta]
]

{ #category : 'change reporting' }
Morph >> privateInvalidateMorph: aMorph [
	"Private. Invalidate the given morph after adding or removing.
	This method is private because a) we're invalidating the morph 'remotely'
	and b) it forces a fullBounds computation which should not be necessary
	for a general morph c) the morph may or may not actually invalidate
	anything (if it's not in the world nothing will happen) and d) the entire
	mechanism should be rewritten."
	aMorph fullBounds.
	aMorph changed
]

{ #category : 'recategorized' }
Morph >> privateMoveBy: delta [
	"Private! Use 'position:' instead."
	| fill border|
	bounds := bounds translateBy: delta.
	fullBounds ifNotNil: [fullBounds := fullBounds translateBy: delta].
	fill := self fillStyle.
	fill isOrientedFill ifTrue: [fill origin: fill origin + delta].
	border := self borderStyle.
	(border hasFillStyle and: [border fillStyle isOrientedFill]) ifTrue: [
		border fillStyle origin: border fillStyle origin + delta]
]

{ #category : 'private' }
Morph >> privateOwner: aMorph [
	"Private! Should only be used by methods that maintain the ower/submorph invariant."

	owner := aMorph
]

{ #category : 'private' }
Morph >> privateRemove: aMorph [
	"Private! Should only be used by methods that maintain the ower/submorph invariant."

	submorphs := submorphs copyWithout: aMorph.
	self layoutChanged
]

{ #category : 'private' }
Morph >> privateSubmorphs [
	"Private! Use 'submorphs' instead."

	^ submorphs
]

{ #category : 'private' }
Morph >> privateSubmorphs: aCollection [
	"Private! Should only be used by methods that maintain the ower/submorph invariant."

	submorphs := aCollection
]

{ #category : 'events - processing' }
Morph >> processEvent: anEvent [
	"Process the given event using the default event dispatcher."
	^self processEvent: anEvent using: self defaultEventDispatcher
]

{ #category : 'events - processing' }
Morph >> processEvent: anEvent using: defaultDispatcher [
	"This is the central entry for dispatching events in morphic. Given some event and a default dispatch strategy, find the right receiver and let him handle it.
	WARNING: This is a powerful hook. If you want to use a different event dispatcher from the default, here is the place to hook it in. Depending on how the dispatcher is written (e.g., whether it calls simply #processEvent: or #processEvent:using:) you can change the dispatch strategy for entire trees of morphs. Similarly, you can disable entire trees of morphs from receiving any events whatsoever. Read the documentation in class MorphicEventDispatcher before playing with it. "
	(self rejectsEvent: anEvent) ifTrue:[^#rejected].
	^defaultDispatcher dispatchEvent: anEvent with: self
]

{ #category : 'accessing' }
Morph >> raisedColor [
	"Return the color to be used for shading raised borders. The
	default is my own color, but it might want to be, eg, my
	owner's color. Whoever's color ends up prevailing, the color
	itself gets the last chance to determine, so that when, for
	example, an InfiniteForm serves as the color, callers won't choke
	on some non-Color object being returned"

	(color isColor and: [ color isTransparent and: [ owner isNotNil ] ])
		ifTrue: [ ^ owner raisedColor ].
	^ color asColor raisedColor
]

{ #category : 'geometry' }
Morph >> referencePosition [
	"Return the current reference position of the receiver"
	| box |
	box := self bounds.
	^box origin + (self rotationCenter * box extent)
]

{ #category : 'geometry' }
Morph >> referencePosition: aPosition [
	"Move the receiver to match its reference position with aPosition"
	| newPos intPos |
	newPos := self position + (aPosition - self referencePosition).
	intPos := newPos asIntegerPoint.
	newPos = intPos
		ifTrue:[self position: intPos]
		ifFalse:[self position: newPos]
]

{ #category : 'geometry' }
Morph >> referencePositionInWorld [

	^ self pointInWorld: self referencePosition
]

{ #category : 'geometry' }
Morph >> referencePositionInWorld: aPoint [
	| localPosition |
	localPosition := owner
		ifNil: [aPoint]
		ifNotNil: [(owner transformFrom: self world) globalPointToLocal: aPoint].

	self referencePosition: localPosition
]

{ #category : 'drawing' }
Morph >> refreshWorld [
	| aWorld |
	(aWorld := self world) ifNotNil: [aWorld displayWorldSafely]
]

{ #category : 'events - processing' }
Morph >> rejectDropEvent: anEvent [
	"This hook allows the receiver to repel a drop operation currently executed. The method is called prior to checking children so the receiver must validate that the event was really designated for it.
	Note that the ordering of the tests below is designed to avoid a (possibly expensive) #fullContainsPoint: test. If the receiver doesn't want to repel the morph anyways we don't need to check after all."
	(self repelsMorph: anEvent contents event: anEvent) ifFalse:[^self]. "not repelled"
	(self fullContainsPoint: anEvent position) ifFalse:[^self]. "not for me"
	"Throw it away"
	anEvent wasHandled: true.
	anEvent contents rejectDropMorphEvent: anEvent
]

{ #category : 'dropping/grabbing' }
Morph >> rejectDropMorphEvent: evt [
	
	"Hook for subclasses"
]

{ #category : 'recategorized' }
Morph >> rejectsEvent: anEvent [
	"Return true to reject the given event.
	Rejecting an event means neither the receiver nor any of it's submorphs will be given any chance to handle it.
	If the event is a mouse wheel event then only reject if the receiver is not visible."

	(anEvent isMouse and: [anEvent isMouseWheel])
		ifTrue: [^self visible not].
	^self isLocked or: [self visible not]
]

{ #category : 'text-anchor' }
Morph >> relativeTextAnchorPosition [
	^self valueOfProperty: #relativeTextAnchorPosition
]

{ #category : 'text-anchor' }
Morph >> relativeTextAnchorPosition: aPoint [
	^self setProperty: #relativeTextAnchorPosition toValue: aPoint
]

{ #category : 'events - removing' }
Morph >> releaseActionMap [
	"Release the action map"

 	self removeProperty: #actionMap
]

{ #category : 'caching' }
Morph >> releaseCachedState [
	"Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'."
	self borderStyle releaseCachedState
]

{ #category : 'events - alarms' }
Morph >> removeAlarm: aSelector [
	"Remove the given alarm"
	| scheduler |
	scheduler := self alarmScheduler.
	scheduler ifNotNil:[scheduler removeAlarm: aSelector for: self]
]

{ #category : 'submorphs - add/remove' }
Morph >> removeAllMorphs [
	| oldMorphs myWorld |
	myWorld := self world.
	(fullBounds isNotNil or:[myWorld isNotNil]) ifTrue:[self invalidRect: self fullBounds].
	submorphs do: [:m | myWorld ifNotNil: [ m outOfWorld: myWorld ]. m privateOwner: nil].
	oldMorphs := submorphs.
	submorphs := EmptyArray.
	oldMorphs do: [ :m | self removedMorph: m ].
	self layoutChanged
]

{ #category : 'submorphs - add/remove' }
Morph >> removeAllMorphsIn: aCollection [
	"greatly speeds up the removal of *lots* of submorphs"
	| set myWorld |
	set := IdentitySet new: aCollection size * 4 // 3.
	aCollection do: [:each | each owner == self ifTrue: [ set add: each]].
	myWorld := self world.
	(fullBounds isNotNil or:[myWorld isNotNil]) ifTrue:[self invalidRect: self fullBounds].
	set do: [:m | myWorld ifNotNil: [ m outOfWorld: myWorld ]. m privateOwner: nil].
	submorphs := submorphs reject: [ :each | set includes: each].
	set do: [ :m | self removedMorph: m ].
	self layoutChanged
]

{ #category : 'updating' }
Morph >> removeDependent: anObject [

	self announcer unsubscribe: anObject
]

{ #category : 'drop shadows' }
Morph >> removeDropShadow [
	self hasDropShadow ifFalse:[^self].
	self changed.
	self hasDropShadow: false.
	self shadowOffset: 0@0.
	self layoutChanged.
	self changed
]

{ #category : 'rotate scale and flex' }
Morph >> removeFlexShell [
	self isFlexed ifTrue: [self owner removeFlexShell]
]

{ #category : 'halos and balloon help' }
Morph >> removeHalo [
	"remove the surrounding halo (if any)"
	self primaryHand removeHaloAround: self
]

{ #category : 'event handling' }
Morph >> removeLink: actionCode [
	self eventHandler ifNotNil:
		[self eventHandler on: actionCode send: nil to: nil]
]

{ #category : 'submorphs - add/remove' }
Morph >> removeMorph: aMorph [
	"Remove the given morph from my submorphs"
	| aWorld |
	aMorph owner == self ifFalse:[^self].
	aWorld := self world.
	aWorld ifNotNil:[
		aMorph outOfWorld: aWorld.
		self privateInvalidateMorph: aMorph.
	].
	self privateRemove: aMorph.
	aMorph privateOwner: nil.
	self removedMorph: aMorph
]

{ #category : 'accessing - properties' }
Morph >> removeProperty: aSymbol [
	"removes the property named aSymbol if it exists"
	extension ifNil:  [^ self].
	extension removeProperty: aSymbol
]

{ #category : 'submorphs - add/remove' }
Morph >> removedMorph: aMorph [
	"Notify the receiver that aMorph was just removed from its children"
]

{ #category : 'naming' }
Morph >> renameTo: aName [
	"Set The morph name."

	self topRendererOrSelf setNameTo: aName.
	^aName
]

{ #category : 'structure' }
Morph >> renderedMorph [
	"This now  gets overridden by rendering morphs."

	^self
]

{ #category : 'dropping/grabbing' }
Morph >> repelsMorph: aMorph event: ev [
	^ false
]

{ #category : 'submorphs - add/remove' }
Morph >> replaceSubmorph: oldMorph by: newMorph [

	| index itsPosition w |
	oldMorph stopStepping.
	itsPosition := oldMorph referencePositionInWorld.
	index := submorphs indexOf: oldMorph.
	oldMorph privateDelete.
	self privateAddMorph: newMorph atIndex: index.
	newMorph referencePositionInWorld: itsPosition.
	(w := newMorph world) ifNotNil: [ newMorph startSteppingSubmorphs ]
]

{ #category : 'event handling' }
Morph >> requestStopTextEditing [

	"Used by a morph to request stop text edition capabilities.
	The rendering backend uses this to stop all text input.
	Use it in pair with requestTextEditingAt:

	By default, propagate the request to the owner.
	If this morph has no owner, do nothing.
	Particular morphs (e.g., the root of the composition hierarchy should override)"
	self owner ifNil: [ ^ self ].
	self owner requestStopTextEditing
]

{ #category : 'event handling' }
Morph >> requestTextEditingAt: aRectangle [

	"Used by a morph to request text edition capabilities.
	The rendering backend can use this information to, for example, open the IME (input-method-editor).
	The rectangle indicates the position where the IME menu could be shown.

	Use it in pair with requestStopTextEditing

	By default, propagate the request to the owner.
	If this morph has no owner, do nothing.
	Particular morphs (e.g., the root of the composition hierarchy should override)"
	self owner ifNil: [ ^ self ].
	self owner requestTextEditingAt: aRectangle
]

{ #category : 'accessing - extension' }
Morph >> resetExtension [
	"reset the extension slot if it is not needed"
	(extension isNotNil and: [extension isDefault]) ifTrue: [extension := nil]
]

{ #category : 'dropping/grabbing' }
Morph >> resetHighlightForDrop [
	self highlightForDrop: false
]

{ #category : 'accessing' }
Morph >> resistsRemoval [
	"Answer whether the receiver is marked as resisting removal"

	^ self hasProperty: #resistsRemoval
]

{ #category : 'accessing' }
Morph >> resistsRemoval: aBoolean [
	"Set the receiver's resistsRemoval property as indicated"

	aBoolean
		ifTrue: [ self setProperty: #resistsRemoval toValue: true ]
		ifFalse: [ self removeProperty: #resistsRemoval ]
]

{ #category : 'menus' }
Morph >> resistsRemovalString [
	"Answer the string to be shown in a menu to represent the
	'resistsRemoval' status"
	^ (self resistsRemoval) -> 'resist being deleted' translated
]

{ #category : 'meta-actions' }
Morph >> resizeFromMenu [
	"Commence an interaction that will resize the receiver"

	self resizeMorph: self activeHand lastEvent
]

{ #category : 'debug and other' }
Morph >> resumeAfterDrawError [

	self changed.
	self removeProperty:#errorOnDraw.
	self removeProperty:#drawError.
	self changed
]

{ #category : 'debug and other' }
Morph >> resumeAfterStepError [
	"Resume stepping after an error has occured."

	self startStepping. "Will #step"
	self removeProperty:#errorOnStep. "Will remove prop only if #step was okay"
]

{ #category : 'layout - properties' }
Morph >> reverseTableCells [
	"Layout specific. This property describes if the cells should be treated in reverse order of submorphs."
	| props |
	props := self layoutProperties.
	^props ifNil:[false] ifNotNil:[props reverseTableCells]
]

{ #category : 'layout - properties' }
Morph >> reverseTableCells: aBool [
	"Layout specific. This property describes if the cells should be treated in reverse order of submorphs."
	self assureTableProperties reverseTableCells: aBool.
	self layoutChanged
]

{ #category : 'geometry' }
Morph >> right [
	" Return the x-coordinate of my right side "
	^ bounds right
]

{ #category : 'geometry' }
Morph >> right: aNumber [
	" Move me so that my right side is at the x-coordinate aNumber. My extent (width & height) are unchanged "

	self position: ((aNumber - bounds width) @ bounds top)
]

{ #category : 'geometry' }
Morph >> rightCenter [

	^ bounds rightCenter
]

{ #category : 'structure' }
Morph >> root [
	"Return the root of the composite morph containing the receiver. The owner of the root is either nil, a WorldMorph, or a HandMorph. If the receiver's owner is nil, the root is the receiver itself. This method always returns a morph."

	(owner isNil or: [owner isWorldOrHandMorph]) ifTrue: [^self].
	^owner root
]

{ #category : 'submorphs - accessing' }
Morph >> rootMorphsAt: aPoint [
	"Return the list of root morphs containing the given point, excluding the receiver.
	ar 11/8/1999: Moved into morph for an incredibly ugly hack in 3D worlds"

	^ self submorphs select: [ :m | (m fullContainsPoint: aPoint) and: [ m isLocked not ] ]
]

{ #category : 'rotate scale and flex' }
Morph >> rotationCenter [
	"Return the rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."
	^self valueOfProperty: #rotationCenter ifAbsent: [0.5@0.5]
]

{ #category : 'rotate scale and flex' }
Morph >> rotationCenter: aPointOrNil [
	"Set the new rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."
	aPointOrNil
		ifNil: [self removeProperty: #rotationCenter]
		ifNotNil:[self setProperty: #rotationCenter toValue: aPointOrNil]
]

{ #category : 'rounding' }
Morph >> roundedCornersString [
	"Answer the string to put in a menu that will invite the user to
	switch to the opposite corner-rounding mode"
	^ (self wantsRoundedCorners
		ifTrue: ['<yes>' translated]
		ifFalse: ['<no>' translated])
		, ('round corners' translated)
]

{ #category : 'layout - properties' }
Morph >> rubberBandCells [
	"Layout specific. This property describes if a parent that is #shrinkWrapped around its children should ignore any #spaceFill children. E.g., when #rubberBandCells is true, the compound layout will always stay at the smallest available size, even though some child may be able to grow."
	| props |
	props := self layoutProperties.
	^props ifNil:[false] ifNotNil:[props rubberBandCells]
]

{ #category : 'layout - properties' }
Morph >> rubberBandCells: aBool [
	"Layout specific. This property describes if a parent that is #shrinkWrapped around its children should ignore any #spaceFill children. E.g., when #rubberBandCells is true, the compound layout will always stay at the smallest available size, even though some child may be able to grow."
	self assureTableProperties rubberBandCells: aBool.
	self layoutChanged
]

{ #category : 'rotate scale and flex' }
Morph >> scale: newScale [
	"Backstop for morphs that don't have to do something special to set their scale"
]

{ #category : 'accessing' }
Morph >> scaleFactor [
	^self valueOfProperty: #scaleFactor ifAbsent: [ 1.0 ]
]

{ #category : 'geometry' }
Morph >> screenLocation [
	"For compatibility only"

	^ self fullBounds origin
]

{ #category : 'geometry' }
Morph >> screenRectangle [
	"For compatibility only"

	^ self fullBounds
]

{ #category : 'selected object' }
Morph >> selectedObject [
	"answer the selected object for the hand or nil is none"
	^ self primaryHand selectedObject
]

{ #category : 'dropping/grabbing' }
Morph >> separateDragAndDrop [
	"Conversion only. Separate the old #dragNDropEnabled into #dragEnabled and #dropEnabled and remove the old property."
	| dnd |
	(self hasProperty: #dragNDropEnabled) ifFalse:[^self].
	dnd := (self valueOfProperty: #dragNDropEnabled) == true.
	self dragEnabled: dnd.
	self dropEnabled: dnd.
	self removeProperty: #dragNDropEnabled
]

{ #category : 'halos and balloon help' }
Morph >> setBalloonText: stringOrText [
	"Set receiver's balloon help text. Pass nil to remove the help."

	self setBalloonText: stringOrText maxLineLength: self theme settings maxBalloonHelpLineLength
]

{ #category : 'halos and balloon help' }
Morph >> setBalloonText: stringOrText maxLineLength: aLength [
	"Set receiver's balloon help text. Pass nil to remove the help."
	(extension isNil and: [stringOrText isNil]) ifTrue: [^ self].
	self assureExtension balloonText:
		(stringOrText ifNotNil: [stringOrText withNoLineLongerThan: aLength])
]

{ #category : 'accessing' }
Morph >> setBorderStyle: aSymbol [
	"Set the border style of my costume"

	| aStyle |
	aStyle := self borderStyleForSymbol: aSymbol.
	aStyle ifNil: [^ self].
	(self canDrawBorder: aStyle)
		ifTrue:
			[self borderStyle: aStyle]
]

{ #category : 'halos and balloon help' }
Morph >> setCenteredBalloonText: aString [
	self setBalloonText: aString
]

{ #category : 'geometry' }
Morph >> setConstrainedPosition: aPoint hangOut: partiallyOutside [
	"Change the position of this morph and and all of its submorphs to aPoint, but don't let me go outside my owner's bounds.  Let me go within two pixels of completely outside if partiallyOutside is true."

	| trialRect delta boundingMorph bRect |
	owner ifNil:[^self].
	trialRect := aPoint extent: self bounds extent.
	boundingMorph := self topRendererOrSelf owner.
	delta := boundingMorph
			ifNil:    [0@0]
			ifNotNil: [
				bRect := partiallyOutside
					ifTrue: [boundingMorph bounds insetBy:
								self extent negated + boundingMorph borderWidth + (2@2)]
					ifFalse: [boundingMorph bounds].
				trialRect amountToTranslateWithin: bRect].
	self position: aPoint + delta.
	self layoutChanged  "So that, eg, surrounding text will readjust"
]

{ #category : 'naming' }
Morph >> setNamePropertyTo: aName [
	"change the receiver's externalName"
	self assureExtension externalName: aName
]

{ #category : 'naming' }
Morph >> setNameTo: aName [
	| nameToUse nameString |
	nameToUse := aName ifNotNil:
					[(nameString := aName asString) notEmpty ifTrue: [nameString] ifFalse: ['*']].
	self setNamePropertyTo: nameToUse	"no Texts here!"
]

{ #category : 'accessing - properties' }
Morph >> setProperties: aList [
	"Set many properties at once from a list of prop, value, prop, value"

	1 to: aList size by: 2 do: [:ii |
		self setProperty: (aList at: ii) toValue: (aList at: ii+1)]
]

{ #category : 'accessing - properties' }
Morph >> setProperty: aSymbol toValue: anObject [
	"change the receiver's property named aSymbol to anObject"
	anObject ifNil: [^ self removeProperty: aSymbol].
	self assureExtension setProperty: aSymbol toValue: anObject
]

{ #category : 'menus' }
Morph >> setRotationCenterFrom: aPoint [
	self rotationCenter: (aPoint - self bounds origin) / self bounds extent asFloatPoint
]

{ #category : 'menus' }
Morph >> setToAdhereToEdge: anEdge [
	anEdge ifNil: [^ self].
	anEdge == #none ifTrue: [^ self removeProperty: #edgeToAdhereTo].
	self setProperty: #edgeToAdhereTo toValue: anEdge
]

{ #category : 'drop shadows' }
Morph >> shadowColor [
	^self valueOfProperty: #shadowColor ifAbsent:[Color black]
]

{ #category : 'drop shadows' }
Morph >> shadowColor: aColor [
	self shadowColor = aColor ifFalse: [self changed].
	self setProperty: #shadowColor toValue: aColor
]

{ #category : 'drop shadows' }
Morph >> shadowOffset [
	"Return the current shadow offset"

	extension ifNil: [^0@0].
	^self valueOfProperty: #shadowOffset ifAbsent:[0@0]
]

{ #category : 'drop shadows' }
Morph >> shadowOffset: aPoint [
	"Set the current shadow offset"
	(aPoint isNil or:[ aPoint isZero ])
		ifTrue:[ self removeProperty: #shadowOffset ]
		ifFalse:[ self setProperty: #shadowOffset toValue: aPoint ]
]

{ #category : 'drop shadows' }
Morph >> shadowPoint: newPoint [
	self changed.
	self shadowOffset: newPoint - self center // 5.
	fullBounds ifNotNil:[fullBounds := self privateFullBounds].
	self changed
]

{ #category : 'geometry' }
Morph >> shiftSubmorphsOtherThan: listNotToShift by: delta [
	| rejectList |
	rejectList := listNotToShift ifNil: [OrderedCollection new].
	(submorphs copyWithoutAll: rejectList) do:
		[:m | m position: (m position + delta)]
]

{ #category : 'shortcuts' }
Morph >> shortcutsHandler [

	^ ShortcutsHandler
]

{ #category : 'testing' }
Morph >> shouldDropOnMouseUp [
	| former |
	former := self formerPosition ifNil:[^false].
	^(former distanceTo: self position) > 10
]

{ #category : 'testing' }
Morph >> shouldFlex [
	^ self isFlexMorph
]

{ #category : 'wiw support' }
Morph >> shouldGetStepsFrom: aWorld [
	^self world == aWorld
]

{ #category : 'drawing' }
Morph >> show [
	"Make sure this morph is on-stage."

	self visible
		ifTrue: [ ^ self ].
	self visible: true.
	self changed
]

{ #category : 'meta-actions' }
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
	Smalltalk tools messageList browse: list title:  'Actions of ' , self printString
]

{ #category : 'halos and balloon help' }
Morph >> showBalloon: msgString [
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."
	| w |
	self showBalloon: msgString hand: ((w := self world) ifNotNil:[w activeHand])
]

{ #category : 'accessing' }
Morph >> simplySetVisible: aBoolean [
	"Set the receiver's visibility property.  This mild circumlocution is because my TransfomationMorph #visible: method would also set the visibility flag of my flexee, which in this case is pointless because it's the flexee that calls this.
	This appears in morph as a backstop for morphs that don't inherit from TFMorph"

	self visible: aBoolean
]

{ #category : 'event testing' }
Morph >> simulateClick [

	self simulateClickWith: MouseEvent redButton
]

{ #category : 'event testing' }
Morph >> simulateClickWith: buttons [
	"Did you know there's #simulateClick (left), #simulateMiddleClick, and #simulateRightClick?
	buttons - look at MouseEvent's class-side for button types"

	| noButtons |
	noButtons := 0.
	{ #mouseDown->buttons. #mouseUp->noButtons } do: [ :type |
		self activeHand handleEvent: (MouseButtonEvent new
			setType: type key
			position: (self pointInWorld: self center) "Some Morphs report local coords"
			which: (noButtons bitXor: buttons)
			buttons: type value
			hand: self activeHand
			stamp: Time millisecondClockValue) ]
]

{ #category : 'event testing' }
Morph >> simulateClickWith: buttons position: position [
	"Did you know there's #simulateClick (left), #simulateMiddleClick, and #simulateRightClick?
	buttons - look at MouseEvent's class-side for button types"

	| noButtons hand |
	noButtons := 0.
	hand := HandMorph new mouseFocus: self; yourself.
	hand privateOwner: self.
	{ #mouseDown->buttons. #mouseUp->noButtons } do: [ :type |
		hand handleEvent: (MouseButtonEvent new
			setType: type key
			position: position "Some Morphs report local coords"
			which: (noButtons bitXor: buttons)
			buttons: type value
			hand: hand
			stamp: Time millisecondClockValue) ]
]

{ #category : 'event testing' }
Morph >> simulateKeyStroke: aCharacter [
	| event |

	event := KeyboardEvent new
				setType: #keystroke
				buttons: 0
				position: 0@0
				keyValue: aCharacter charCode
				charCode: aCharacter charCode
				hand: self activeHand
				stamp: 0.
	self keyStroke: event
]

{ #category : 'event testing' }
Morph >> simulateKeyStrokes: aString [

	aString do: [:c | self simulateKeyStroke: c ]
]

{ #category : 'event testing' }
Morph >> simulateMiddleClick [

	self simulateClickWith: MouseEvent blueButton
]

{ #category : 'event testing' }
Morph >> simulateMiddleMouseDown [
	"Simulate mouse button down with the left button (red button for left, blue for middle, yellow for right)"
	self simulateMouseDownWith: MouseEvent blueButton
]

{ #category : 'event testing' }
Morph >> simulateMiddleMouseUp [
	"Simulate mouse button up with the left button (red button for left, blue for middle, yellow for right)"
	self simulateMouseUpWith: MouseEvent blueButton
]

{ #category : 'event testing' }
Morph >> simulateMouseDown [
	"Simulate mouse button down with the left button (red button for left, blue for middle, yellow for right)"
	self simulateMouseDownWith: MouseEvent redButton
]

{ #category : 'event testing' }
Morph >> simulateMouseDownWith: buttons [
	"Simulate mouse button down with a given button"

	self handleEvent: (MouseButtonEvent new
		setType: #mouseDown
		position: (self pointInWorld: self center) "Some Morphs report local coords"
		which:  buttons
		buttons: buttons
		hand: self activeHand
		stamp: Time millisecondClockValue)
]

{ #category : 'event testing' }
Morph >> simulateMouseMove: aPointStart to: aPointEnd [
"Simulates a mouse moving from a given point to a destinationpoint"
	| event |
	event := MouseMoveEvent  new
				setType: #mouseMove
				startPoint: aPointStart
				endPoint: aPointEnd
				trail: nil
				buttons: 0
				hand: self activeHand
				stamp: 0.
	self mouseMove: event
]

{ #category : 'event testing' }
Morph >> simulateMouseUp [
	"Simulate mouse button up with the left button (red button for left, blue for middle, yellow for right)"
	self simulateMouseUpWith: MouseEvent redButton
]

{ #category : 'event testing' }
Morph >> simulateMouseUpWith: buttons [
	"Simulate mouse button up with a given button"

	self handleEvent: (MouseButtonEvent new
		setType: #mouseUp
		position: (self pointInWorld: self center) "Some Morphs report local coords"
		which:  buttons
		buttons: buttons
		hand: self activeHand
		stamp: Time millisecondClockValue)
]

{ #category : 'event testing' }
Morph >> simulateRightClick [

	self simulateClickWith: MouseEvent yellowButton
]

{ #category : 'event testing' }
Morph >> simulateRightMouseDown [
	"Simulate mouse button down with the left button (red button for left, blue for middle, yellow for right)"
	self simulateMouseDownWith: MouseEvent yellowButton
]

{ #category : 'event testing' }
Morph >> simulateRightMouseUp [
	"Simulate mouse button up with the left button (red button for left, blue for middle, yellow for right)"
	self simulateMouseUpWith: MouseEvent yellowButton
]

{ #category : 'menus' }
Morph >> snapToEdgeIfAppropriate [
	| edgeSymbol oldBounds aWorld |
	(edgeSymbol := self valueOfProperty: #edgeToAdhereTo) ifNotNil:
		[oldBounds := bounds.
		self adhereToEdge: edgeSymbol.
		bounds ~= oldBounds ifTrue: [(aWorld := self world) ifNotNil: [aWorld viewBox ifNotNil:
			[aWorld displayWorld]]]]
]

{ #category : 'layout - properties' }
Morph >> spaceFillWeight [
	"Layout specific. This property describes the relative weight that
	should be given to the receiver when extra space is distributed
	between different #spaceFill cells."

	^ self
		valueOfProperty: #spaceFillWeight
		ifAbsent: [1]
]

{ #category : 'layout - properties' }
Morph >> spaceFillWeight: aNumber [
	"Layout specific. This property describes the relative weight that should be given to the receiver when extra space is distributed between different #spaceFill cells."
	aNumber = 1
		ifTrue:[self removeProperty: #spaceFillWeight]
		ifFalse:[self setProperty: #spaceFillWeight toValue: aNumber].
	self layoutChanged
]

{ #category : 'stepping and presenter' }
Morph >> start [
	"Start running my script. For ordinary morphs, this means start stepping."

	self startStepping
]

{ #category : 'event handling' }
Morph >> startDrag: evt [
	"Handle a double-click event. This message is only sent to clients that request it by sending #waitForClicksOrDrag:event: to the initiating hand in their mouseDown: method. This default implementation does nothing."

	self eventHandler ifNotNil:
		[self eventHandler startDrag: evt fromMorph: self]
]

{ #category : 'dropping/grabbing' }
Morph >> startDrag: anItem with: anObject [
	self currentHand attachMorph: anObject
]

{ #category : 'stepping and presenter' }
Morph >> startStepping [
	"Start getting sent the 'step' message."
	self startStepping: #stepAt: at: Time millisecondClockValue arguments: nil stepTime: nil
]

{ #category : 'stepping and presenter' }
Morph >> startStepping: aSelector at: scheduledTime arguments: args stepTime: stepTime [
	"Start stepping the receiver"
	| w |
	w := self world.
	w ifNotNil: [
		w startStepping: self at: scheduledTime selector: aSelector arguments: args stepTime: stepTime.
		self changed]
]

{ #category : 'stepping and presenter' }
Morph >> startSteppingSelector: aSelector [
	"Start getting sent the 'step' message."
	self startStepping: aSelector at: Time millisecondClockValue arguments: nil stepTime: nil
]

{ #category : 'stepping and presenter' }
Morph >> startSteppingSubmorphs [
	"Ensure that all submorphs of the given morph that want to be stepped are added to the step list.   Typically used after adding a morph to the world."

	self allMorphsDo: [:submorph |
		submorph wantsSteps ifTrue: [submorph arrangeToStartSteppingIn: submorph world].
	]
]

{ #category : 'stepping and presenter' }
Morph >> step [
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The time between steps is specified by this morph's answer to the stepTime message.  The generic version dispatches control to the player, if any.  The nasty circumlocation about owner's transformation is necessitated by the flexing problem that the player remains in the properties dictionary both of the flex and the real morph.  In the current architecture, only the top renderer's pointer to the player should actually be honored for the purpose of firing."
]

{ #category : 'stepping and presenter' }
Morph >> stepAt: millisecondClockValue [
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The time between steps is specified by this morph's answer to the stepTime message.
	The millisecondClockValue parameter gives the value of the millisecond clock at the moment of dispatch.
	Default is to dispatch to the parameterless step method for the morph, but this protocol makes it possible for some morphs to do differing things depending on the clock value"

	self step
]

{ #category : 'stepping and presenter' }
Morph >> stepTime [

	^ 1000 "milliseconds -- default backstop for objects serving as models of system windows"
]

{ #category : 'menus' }
Morph >> stickinessString [
	"Answer the string to be shown in a menu to represent the stickiness status"

	^ (self isSticky) ->  'resist being picked up' translated
]

{ #category : 'accessing' }
Morph >> sticky: aBoolean [
	"change the receiver's sticky property"
	extension sticky: aBoolean
]

{ #category : 'stepping and presenter' }
Morph >> stop [
	"Stop running my script. For ordinary morphs, this means stop stepping."

	self stopStepping
]

{ #category : 'stepping and presenter' }
Morph >> stopStepping [
	"Stop getting sent the 'step' message."

	| w |
	w := self world.
	w ifNotNil: [w stopStepping: self]
]

{ #category : 'stepping and presenter' }
Morph >> stopSteppingSelector: aSelector [
	"Stop getting sent the given message."
	| w |
	w := self world.
	w ifNotNil: [w stopStepping: self selector: aSelector]
]

{ #category : 'stepping and presenter' }
Morph >> stopSteppingSelfAndSubmorphs [
	self allMorphsDo: [:m | m stopStepping]
]

{ #category : 'submorphs - accessing' }
Morph >> submorphAfter [
	"Return the submorph after (behind) me, or nil"
	| ii |
	owner ifNil: [^ nil].
	^ (ii := owner submorphIndexOf: self) = owner submorphs size
		ifTrue: [nil]
		ifFalse: [owner submorphs at: ii+1]
]

{ #category : 'submorphs - accessing' }
Morph >> submorphBefore [
	"Return the submorph after (behind) me, or nil"
	| ii |
	owner ifNil: [^ nil].
	^ (ii := owner submorphIndexOf: self) = 1
		ifTrue: [nil]
		ifFalse: [owner submorphs at: ii-1]
]

{ #category : 'layout' }
Morph >> submorphBounds [
	"Private. Compute the actual full bounds of the receiver"
	| box |
	submorphs do: [:m | | subBox |
		(m visible) ifTrue: [
			subBox := m fullBounds.
			box
				ifNil:[box := subBox copy]
				ifNotNil:[box := box quickMerge: subBox]]].
	box ifNil:[^self bounds]. "e.g., having submorphs but not visible"
	^ box origin asIntegerPoint corner: box corner asIntegerPoint
]

{ #category : 'submorphs - accessing' }
Morph >> submorphCount [

	^ submorphs size
]

{ #category : 'submorphs - add/remove' }
Morph >> submorphIndexOf: aMorph [
	"Assuming aMorph to be one of my submorphs, answer where it occurs in my submorph list"

	^ submorphs indexOf: aMorph ifAbsent: [nil]
]

{ #category : 'submorphs - accessing' }
Morph >> submorphThat: block1 ifNone: block2 [
	^ submorphs detect: [:m | (block1 value: m) == true] ifNone: block2
]

{ #category : 'submorphs - accessing' }
Morph >> submorphWithProperty: aSymbol [
	^ submorphs detect: [:aMorph | aMorph hasProperty: aSymbol] ifNone: [nil]
]

{ #category : 'submorphs - accessing' }
Morph >> submorphs [
	"This method returns my actual submorphs collection. Modifying the collection directly could be dangerous; make a copy if you need to alter it."
	^ submorphs
]

{ #category : 'submorphs - accessing' }
Morph >> submorphsBehind: aMorph do: aBlock [
	| behind |
	behind := false.
	submorphs do:
		[:m | m == aMorph ifTrue: [behind := true]
						ifFalse: [behind ifTrue: [aBlock value: m]]]
]

{ #category : 'submorphs - accessing' }
Morph >> submorphsDo: aBlock [
	submorphs do: aBlock
]

{ #category : 'submorphs - accessing' }
Morph >> submorphsInFrontOf: aMorph do: aBlock [
	| behind |
	behind := false.
	submorphs do:
		[:m | m == aMorph ifTrue: [behind := true]
						ifFalse: [behind ifFalse: [aBlock value: m]]]
]

{ #category : 'submorphs - accessing' }
Morph >> submorphsReverseDo: aBlock [

	submorphs reverseDo: aBlock
]

{ #category : 'submorphs - accessing' }
Morph >> submorphsSatisfying: aBlock [
	^ submorphs select: [:m | (aBlock value: m) == true]
]

{ #category : 'event handling' }
Morph >> tabAmongFields [
	^ self theme settings tabAmongFields
		or: [self hasProperty: #tabAmongFields]
]

{ #category : 'card in a stack' }
Morph >> tabHitWithEvent: anEvent [
	"The tab key was hit.  The keyboard focus has referred this event to me, though this perhaps seems rather backwards.  Anyway, the assumption is that I have the property #tabAmongFields, so now the task is to tab to the next field."

	| currentFocus fieldList anIndex itemToHighlight |
	currentFocus := anEvent hand keyboardFocus.
	fieldList := self allMorphs select:
		[:aMorph | (aMorph wouldAcceptKeyboardFocusUponTab) and: [aMorph isLocked not]].

	fieldList isEmpty ifTrue:[^ self].

	anIndex := fieldList indexOf: currentFocus ifAbsent: [nil].
	itemToHighlight := fieldList atWrap:
		(anIndex ifNotNil: [anEvent shiftPressed ifTrue: [anIndex - 1] ifFalse: [anIndex + 1]]
				ifNil: [1]).
	anEvent hand newKeyboardFocus: itemToHighlight. "really???"
	itemToHighlight editor selectAll.
	itemToHighlight invalidRect: itemToHighlight bounds
]

{ #category : 'accessing - backstop' }
Morph >> target: aMorph [
"Morphs with targets will override. This backstop does nothing."
"This is here because targeting meta-actions are taken at morph level.
Do not remove."
]

{ #category : 'meta-actions' }
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

{ #category : 'text-anchor' }
Morph >> textAnchorType [
	^self valueOfProperty: #textAnchorType ifAbsent:[#document]
]

{ #category : 'text-anchor' }
Morph >> textAnchorType: aSymbol [
	^ aSymbol == #document
		ifTrue: [ self removeProperty: #textAnchorType ]
		ifFalse: [ self setProperty: #textAnchorType toValue: aSymbol ]
]

{ #category : 'event handling' }
Morph >> textEdition: aTextEditionEvent [
]

{ #category : 'event handling' }
Morph >> textInput: aTextInputEvent [
]

{ #category : 'theme' }
Morph >> theme [
	"Answer the current theme for the receiver."

	(self valueOfProperty: #theme) ifNotNil: [:t | ^ t].
	^(self owner ifNil: [self class]) theme
]

{ #category : 'theme' }
Morph >> theme: aUITheme [
	"Set the current theme for the receiver and propagate theme changed to submorphs."

	self theme = aUITheme ifFalse: [
		self setProperty: #theme toValue: aUITheme.
		"we do not invoke basicTheme: to avoid checking twice if the theme changed or not"
		self themeChanged]
]

{ #category : 'theme' }
Morph >> themeChanged [
	"The current theme has changed.
	Update any dependent visual aspects."

	self submorphsDo: [:m | m themeChanged].
	self changed
]

{ #category : 'tiling' }
Morph >> tileBottom [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max leftCenter corner: max corner)
]

{ #category : 'tiling' }
Morph >> tileBottomLeft [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max leftCenter corner: max bottomCenter)
]

{ #category : 'tiling' }
Morph >> tileBottomRight [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max center corner: max corner)
]

{ #category : 'tiling' }
Morph >> tileCenter [

	| max initial |
	max := RealEstateAgent maximumUsableArea.
	initial := self initialExtent.
	self bounds: ((max center - (initial/2)) extent: initial)
]

{ #category : 'tiling' }
Morph >> tileFull [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: max
]

{ #category : 'tiling' }
Morph >> tileLeft [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topLeft corner: max bottomCenter)
]

{ #category : 'tiling' }
Morph >> tileRight [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topCenter corner: max corner)
]

{ #category : 'tiling' }
Morph >> tileTop [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topLeft corner: max rightCenter)
]

{ #category : 'tiling' }
Morph >> tileTopLeft [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topLeft corner: max center)
]

{ #category : 'tiling' }
Morph >> tileTopRight [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topCenter corner: max rightCenter)
]

{ #category : 'rounding' }
Morph >> toggleCornerRounding [
	self cornerStyle == #rounded
		ifTrue: [self cornerStyle: #square]
		ifFalse: [self cornerStyle: #rounded].
	self changed
]

{ #category : 'dropping/grabbing' }
Morph >> toggleDragNDrop [
	"Toggle this morph's ability to add and remove morphs via drag-n-drop."

		self enableDragNDrop: self dragNDropEnabled not
]

{ #category : 'drop shadows' }
Morph >> toggleDropShadow [
	self hasDropShadow
		ifTrue:[self removeDropShadow]
		ifFalse:[self addDropShadow]
]

{ #category : 'accessing' }
Morph >> toggleLocked [

	self lock: self isLocked not
]

{ #category : 'accessing' }
Morph >> toggleResistsRemoval [
	"Toggle the resistsRemoval property"

	self resistsRemoval
		ifTrue:
			[self removeProperty: #resistsRemoval]
		ifFalse:
			[self setProperty: #resistsRemoval toValue: true]
]

{ #category : 'accessing' }
Morph >> toggleStickiness [
	"togle the receiver's Stickiness"
	extension ifNil: [^ self beSticky].
	extension sticky: extension sticky not
]

{ #category : 'geometry' }
Morph >> top [
	" Return the y-coordinate of my top side "

	^ bounds top
]

{ #category : 'geometry' }
Morph >> top: aNumber [
	" Move me so that my top is at the y-coordinate aNumber. My extent (width & height) are unchanged "

	self position: (bounds left @ aNumber)
]

{ #category : 'geometry' }
Morph >> topCenter [

	^ bounds topCenter
]

{ #category : 'geometry' }
Morph >> topLeft [

	^ bounds topLeft
]

{ #category : 'geometry' }
Morph >> topLeft: aPoint [
	" Move me so that my top left corner is at aPoint. My extent (width & height) are unchanged "

	self position: aPoint
]

{ #category : 'base - worlds' }
Morph >> topPasteUp [
	"If the receiver is in a world, return that; otherwise return the outermost pasteup morph"
	^ self outermostMorphThat: [:m | m isKindOf: PasteUpMorph]
]

{ #category : 'structure' }
Morph >> topRendererOrSelf [
	"Answer the topmost renderer for this morph, or this morph itself if it has no renderer. See the comment in Morph>isRenderer."

	| top topsOwner |
	owner ifNil: [^self].
	self isWorldMorph ifTrue: [^self].	"ignore scaling of this world"
	top := self.
	topsOwner := top owner.
	[topsOwner isNotNil and: [topsOwner isRenderer]] whileTrue:
			[top := topsOwner.
			topsOwner := top owner].
	^top
]

{ #category : 'geometry' }
Morph >> topRight [

	^ bounds topRight
]

{ #category : 'geometry' }
Morph >> topRight: aPoint [
	" Move me so that my top right corner is at aPoint. My extent (width & height) are unchanged "

	self position: ((aPoint x - bounds width) @ (aPoint y))
]

{ #category : 'halos and balloon help' }
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

{ #category : 'menus' }
Morph >> transferStateToRenderer: aRenderer [
	"Transfer knownName, and visible over to aRenderer, which is being imposed above me as a transformation shell"

	aRenderer simplySetVisible: self visible
]

{ #category : 'event handling' }
Morph >> transformFrom: uberMorph [
	"Return a transform to be used to map coordinates in a morph above me into my childrens coordinates, or vice-versa. This is used to support scrolling, scaling, and/or rotation. This default implementation just returns my owner's transform or the identity transform if my owner is nil.
	Note:  This method cannot be used to map into the receiver's coordinate system!"

	(self == uberMorph or: [owner isNil]) ifTrue: [^IdentityTransform new].
	^owner transformFrom: uberMorph
]

{ #category : 'event handling' }
Morph >> transformFromOutermostWorld [
	"Return a transform to map world coordinates into my local coordinates"

	"self isWorldMorph ifTrue: [^ MorphicTransform identity]."
	^ self transformFrom: self outermostWorldMorph
]

{ #category : 'event handling' }
Morph >> transformFromWorld [
	"Return a transform to map world coordinates into my local coordinates"

	^ self transformFrom: nil
]

{ #category : 'geometry' }
Morph >> transformedBy: aTransform [
	aTransform isIdentity ifTrue:[^self].
	aTransform isPureTranslation ifTrue:[
		^self position: (aTransform localPointToGlobal: self position).
	].
	^self addFlexShell transformedBy: aTransform
]

{ #category : 'events - processing' }
Morph >> transformedFrom: uberMorph [
	"Return a transform to map coordinates of uberMorph, a morph above me in my owner chain, into the coordinates of MYSELF not any of my children."
	"self flag: #arNote." "rename this method"
	owner ifNil:[^IdentityTransform basicNew].
	^ (owner transformFrom: uberMorph)
]

{ #category : 'utilities' }
Morph >> transparentSpacerOfSize: aPoint [
	^ (Morph new extent: aPoint) color: Color transparent
]

{ #category : 'accessing' }
Morph >> unlock [
	self lock: false
]

{ #category : 'accessing' }
Morph >> unlockContents [
	self submorphsDo:
		[:m | m unlock]
]

{ #category : 'updating' }
Morph >> update: anAspect [

	^ self
]

{ #category : 'events - accessing' }
Morph >> updateableActionMap [
	"Answer an updateable action map, saving it in my #actionMap property"

	| actionMap |
	self assureExtension.
	actionMap := extension actionMap.
	actionMap ifNil:
		[actionMap := self createActionMap.
		extension actionMap: actionMap].
	^actionMap
]

{ #category : 'visual properties' }
Morph >> useDefaultFill [
	"Make receiver use a solid fill style (e.g., a simple color)"
	self fillStyle: self defaultColor
]

{ #category : 'rounding' }
Morph >> useRoundedCorners [
	self cornerStyle: #rounded
]

{ #category : 'visual properties' }
Morph >> useSolidFill [
	"Make receiver use a solid fill style (e.g., a simple color)"
	self fillStyle isSolidFill ifTrue:[^self]. "Already done"
	self fillStyle: self fillStyle asColor. "Try minimizing changes"
]

{ #category : 'accessing' }
Morph >> userString [
	"Do I have a text string to be searched on?"

	^ nil
]

{ #category : 'layout - properties' }
Morph >> vResizeToFit: aBoolean [
	aBoolean ifTrue:[
		self vResizing: #shrinkWrap.
	] ifFalse:[
		self vResizing: #rigid.
	]
]

{ #category : 'layout - properties' }
Morph >> vResizing [
	"Layout specific. This property describes how the receiver should be resized with respect to its owner and its children. Possible values are:
		#rigid			-	do not resize the receiver
		#spaceFill		-	resize to fill owner's available space
		#shrinkWrap	- resize to fit children
	"
	| props |
	props := self layoutProperties.
	^props ifNil:[#rigid] ifNotNil:[props vResizing]
]

{ #category : 'layout - properties' }
Morph >> vResizing: aSymbol [
	"Layout specific. This property describes how the receiver should be resized with respect to its owner and its children. Possible values are:
		#rigid			-	do not resize the receiver
		#spaceFill		-	resize to fill owner's available space
		#shrinkWrap	- resize to fit children
	"
	self assureLayoutProperties vResizing: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> vResizingString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self vResizing
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol [

	"answer the value of the receiver's property named aSymbol"

	^ extension ifNotNil: [ :ext | ext valueOfProperty: aSymbol ]
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol ifAbsent: aBlock [
	"if the receiver possesses a property of the given name, answer
	its value. If not then evaluate aBlock and answer the result of
	this block evaluation"
	^ extension
		ifNotNil: [extension valueOfProperty: aSymbol ifAbsent: aBlock]
		ifNil: [aBlock value]
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol ifAbsentPut: aBlock [
	"If the receiver possesses a property of the given name, answer
	its value. If not, then create a property of the given name, give
	it the value obtained by evaluating aBlock, then answer that
	value"
	^ self assureExtension valueOfProperty: aSymbol ifAbsentPut: aBlock
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol ifPresentDo: aBlock [
	"If the receiver has a property of the given name, evaluate
	aBlock on behalf of the value of that property"
	extension ifNil:  [^ self].
	^ aBlock value: (extension valueOfProperty: aSymbol ifAbsent: [^ self])
]

{ #category : 'copying' }
Morph >> veryDeepCopyWith: deepCopier [
	"Copy me and the entire tree of objects I point to.  An object in the tree twice is copied once, and both references point to him.  deepCopier holds a dictionary of objects we have seen.  See veryDeepInner:, veryDeepFixupWith:"

	self prepareToBeSaved.
	^ super veryDeepCopyWith: deepCopier
]

{ #category : 'copying' }
Morph >> veryDeepFixupWith: deepCopier [
	"If some fields were weakly copied, fix new copy here."

	"super veryDeepFixupWith: deepCopier.	Object has no fixups, so don't call it"

	"If my owner is being duplicated too, then store his duplicate.
	 If I am owned outside the duplicated tree, then I am no longer owned!"
	owner := deepCopier references at: owner ifAbsent: [nil]
]

{ #category : 'copying' }
Morph >> veryDeepInner: deepCopier [
	"The inner loop, so it can be overridden when a field should not
	be traced."
	"super veryDeepInner: deepCopier.	know Object has no inst vars"
	bounds := bounds shallowCopy.
	"Points are shared with original"
	"owner := owner.	special, see veryDeepFixupWith:"
	submorphs := submorphs veryDeepCopyWith: deepCopier.
	"each submorph's fixup will install me as the owner"
	"fullBounds := fullBounds.	fullBounds is shared with original!"
	color := color veryDeepCopyWith: deepCopier.
	"color, if simple, will return self. may be complex"
	extension := (extension veryDeepCopyWith: deepCopier)
]

{ #category : 'base - worlds' }
Morph >> viewBox [
	^ self pasteUpMorph viewBox
]

{ #category : 'drawing' }
Morph >> visible [
	"answer whether the receiver is visible"
	^ extension ifNil: [ true ] ifNotNil: [ :ext | ext visible ]
]

{ #category : 'accessing' }
Morph >> visibleClearArea [
	"Answer the receiver visible clear area. The intersection
	between the clear area and the viewbox."
	^ self viewBox intersect: self clearArea ifNone: [ (0@0 corner: 0@0 ) ]
]

{ #category : 'halos and balloon help' }
Morph >> wantsBalloon [
	"Answer true if receiver wants to show a balloon help text is a few moments."

	^ (self balloonText isNotNil) and: [self balloonHelpEnabled]
]

{ #category : 'halos and balloon help' }
Morph >> wantsDirectionHandles [
	^self valueOfProperty: #wantsDirectionHandles ifAbsent:[false]
]

{ #category : 'halos and balloon help' }
Morph >> wantsDirectionHandles: aBool [
	self setProperty: #wantsDirectionHandles toValue: aBool
]

{ #category : 'event handling' }
Morph >> wantsDropFiles: anEvent [
	"Return true if the receiver wants files dropped from the OS."
	^false
]

{ #category : 'dropping/grabbing' }
Morph >> wantsDroppedMorph: aMorph event: evt [
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self."

	^self dropEnabled
]

{ #category : 'halos and balloon help' }
Morph >> wantsHaloFor: aSubMorph [
	^ false
]

{ #category : 'halos and balloon help' }
Morph >> wantsHaloFromClick [
	^ true
]

{ #category : 'halos and balloon help' }
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

{ #category : 'event handling' }
Morph >> wantsKeyboardFocusFor: aSubmorph [
	"Answer whether a plain mouse click on aSubmorph, a text-edit-capable thing, should result in a text selection there"
	^ false
]

{ #category : 'halos and balloon help' }
Morph >> wantsRecolorHandle [
	"Answer whether the receiver would like a recoloring halo handle to be put up.  Since this handle also presently affords access to the property-sheet, it is presently always allowed, even though SketchMorphs don't like regular recoloring"

	^ true
]

{ #category : 'rounding' }
Morph >> wantsRoundedCorners [
	"Return true if the receiver wants its corners rounded"
	^ self cornerStyle == #rounded
]

{ #category : 'stepping and presenter' }
Morph >> wantsSteps [
	"Return true if the receiver overrides the default Morph step method."
	"Details: Find first class in superclass chain that implements #step and return true if it isn't class Morph."

	| c |
	c := self class.
	[c includesSelector: #step] whileFalse: [c := c superclass].
	^ c ~= Morph
]

{ #category : 'accessing' }
Morph >> wantsToBeCachedByHand [
	"Return true if the receiver wants to be cached by the hand when it is dragged around.
	Note: The default implementation queries all submorphs since subclasses may have shapes that do not fill the receiver's bounds completely."

	self isTranslucentButNotTransparent ifTrue: [ ^ false ].
	self
		submorphsDo: [ :m |
			m wantsToBeCachedByHand
				ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'dropping/grabbing' }
Morph >> wantsToBeDroppedInto: aMorph [
	"Return true if it's okay to drop the receiver into aMorph. This check is symmetric to #wantsDroppedMorph:event: to give both parties a chance of figuring out whether they like each other."
	^true
]

{ #category : 'accessing' }
Morph >> wantsToBeTopmost [
	"Answer if the receiver want to be one of the topmost objects in its owner"
	^ false
]

{ #category : 'menu' }
Morph >> wantsYellowButtonMenu [
	"Answer true if the receiver wants a yellow button menu"
	self
		valueOfProperty: #wantsYellowButtonMenu
		ifPresentDo: [:value | ^ value].
	self isInSystemWindow ifTrue: [^ false].
	^ self defaultYellowButtonMenuEnabled
]

{ #category : 'menu' }
Morph >> wantsYellowButtonMenu: aBoolean [
	"Change the receiver to wants or not a yellow button menu"
	self setProperty: #wantsYellowButtonMenu toValue: aBoolean
]

{ #category : 'geometry' }
Morph >> width [

	^ bounds width
]

{ #category : 'geometry' }
Morph >> width: aNumber [
	" Set my width; my position (top-left corner) and height will remain the same "

	self extent: aNumber asInteger@self height
]

{ #category : 'base - widgets' }
Morph >> widthToDisplayInList: aList [

	^ self minExtent x
]

{ #category : 'event handling' }
Morph >> windowEvent: anEvent [
	"Host window event"
]

{ #category : 'structure' }
Morph >> withAllOwners [
	"Return the receiver and all its owners"

	^ Array streamContents: [:strm | self withAllOwnersDo: [:m | strm nextPut: m]]
]

{ #category : 'structure' }
Morph >> withAllOwnersDo: aBlock [
	"Evaluate aBlock with the receiver and all of its owners"
	aBlock value: self.
	owner ifNotNil:[^owner withAllOwnersDo: aBlock]
]

{ #category : 'structure' }
Morph >> world [
	^owner ifNotNil: [owner world]
]

{ #category : 'geometry' }
Morph >> worldBounds [
	^ self world bounds
]

{ #category : 'event handling' }
Morph >> wouldAcceptKeyboardFocus [
	"Answer whether a plain mouse click on the receiver should result in a text selection there"
	^ false
]

{ #category : 'event handling' }
Morph >> wouldAcceptKeyboardFocusUponTab [
	"Answer whether the receiver is in the running as the new keyboard focus if the tab key were hit at a meta level.  This provides the leverage for tabbing among fields of a card, for example."

	^ false
]

{ #category : 'layout - properties' }
Morph >> wrapCentering [
	"Layout specific. This property describes how the rows/columns in a list-like layout should be centered.
		#topLeft - center at start of secondary direction
		#bottomRight - center at end of secondary direction
		#center - center in the middle of secondary direction
		#justified - insert extra space inbetween rows/columns
	"
	| props |
	props := self layoutProperties.
	^props ifNil:[#topLeft] ifNotNil:[props wrapCentering]
]

{ #category : 'layout - properties' }
Morph >> wrapCentering: aSymbol [
	"Layout specific. This property describes how the rows/columns in a list-like layout should be centered.
		#topLeft - center at start of secondary direction
		#bottomRight - center at end of secondary direction
		#center - center in the middle of secondary direction
		#justified - insert extra space inbetween rows/columns
	"
	self assureTableProperties wrapCentering: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> wrapCenteringString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self wrapCentering
]

{ #category : 'layout - properties' }
Morph >> wrapDirection [
	"Layout specific. This property describes the direction along which a list-like layout should be wrapped. Possible values are:
		#leftToRight
		#rightToLeft
		#topToBottom
		#bottomToTop
		#none
	indicating in which direction wrapping should occur. This direction must be orthogonal to the list direction, that is if listDirection is #leftToRight or #rightToLeft then wrapDirection must be #topToBottom or #bottomToTop and vice versa."
	| props |
	props := self layoutProperties.
	^props ifNil:[#none] ifNotNil:[props wrapDirection]
]

{ #category : 'layout - properties' }
Morph >> wrapDirection: aSymbol [
	"Layout specific. This property describes the direction along which a list-like layout should be wrapped. Possible values are:
		#leftToRight
		#rightToLeft
		#topToBottom
		#bottomToTop
		#none
	indicating in which direction wrapping should occur. This direction must be orthogonal to the list direction, that is if listDirection is #leftToRight or #rightToLeft then wrapDirection must be #topToBottom or #bottomToTop and vice versa."
	self assureTableProperties wrapDirection: aSymbol.
	self layoutChanged
]

{ #category : 'layout - properties' }
Morph >> wrapDirectionString: aSymbol [
	^self layoutMenuPropertyString: aSymbol from: self wrapDirection
]

{ #category : 'event handling' }
Morph >> yellowButtonActivity: shiftState [
	"Find me or my outermost owner that has items to add to a
	yellow button menu.
	shiftState is true if the shift was pressed.
	Otherwise, build a menu that contains the contributions from
	myself and my interested submorphs,
	and present it to the user."
	| menu |
	self isWorldMorph
		ifFalse: [| outerOwner |
			outerOwner := self outermostOwnerWithYellowButtonMenu.
			outerOwner
				ifNil: [^ false].
			outerOwner == self
				ifFalse: [^ outerOwner yellowButtonActivity: shiftState]].
	menu := self buildYellowButtonMenu: self activeHand.
	menu
		addTitle: self externalName
		icon: (self iconOrThumbnailOfSize: 28).
	menu popUpInWorld: self currentWorld.
	^ true
]
