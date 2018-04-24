"
I represent an item in a menu.



Instance variables:
	isEnabled 	<Boolean>	True if the menu item can be executed.
	subMenu 	<MenuMorph | nil>	The submenu to activate automatically when the user mouses over the item.
	isSelected 	<Boolean>	True if the item is currently selected.
	target 		<Object>		The target of the associated action.
	selector 		<Symbol>	The associated action.
	arguments 	<Array>		The arguments for the associated action.
	icon		<Form | nil>	An optional icon form to be displayed to my left.


"
Class {
	#name : #MenuItemMorph,
	#superclass : #StringMorph,
	#instVars : [
		'isEnabled',
		'subMenu',
		'isSelected',
		'target',
		'selector',
		'arguments',
		'icon'
	],
	#classVars : [
		'SubMenuMarker'
	],
	#category : #'Morphic-Base-Menus'
}

{ #category : #initialization }
MenuItemMorph class >> initialize [
	"MenuItemMorph initialize"

	| f |
	f := Form
		extent: 5@9
		fromArray: #(2147483648 3221225472 3758096384 4026531840 4160749568 4026531840 3758096384 3221225472 2147483648)
		offset: 0@0.
	SubMenuMarker := ColorForm mappingWhiteToTransparentFrom: f.

]

{ #category : #grabbing }
MenuItemMorph >> aboutToBeGrabbedBy: aHand [
	"Don't allow the receiver to act outside a Menu"
	| menu box |
	(owner notNil and:[owner hasSubmorphs ]) ifTrue:[
		"I am a lonely menuitem already; just grab my owner"
		owner stayUp: true.
		^owner aboutToBeGrabbedBy: aHand].
	box := self bounds.
	menu := UIManager default newMenuIn: self for: nil.
	menu addMorphFront: self.
	menu bounds: box.
	menu stayUp: true.
	self isSelected: false.
	^menu
]

{ #category : #events }
MenuItemMorph >> activateOwnerMenu: evt [
	"Activate our owner menu; e.g., pass control to it.
	 If it does not contain the pointer position, pass it through the owner chain."

	| popUpOwner |
	(owner isNil or: [ owner isMenuMorph not ]) ifTrue: [ ^ false ].
	(owner fullContainsPoint: evt position)
		ifTrue: [ 
			owner activate: evt.
			^ true ].
	popUpOwner := owner popUpOwner.
	[ popUpOwner ]
		whileNotNil: [ 
			(popUpOwner owner fullContainsPoint: evt position)
				ifTrue: [ 
					popUpOwner owner activate: evt.
					^ true ]
				ifFalse: [ popUpOwner := popUpOwner owner popUpOwner ] ].
	^ false
]

{ #category : #events }
MenuItemMorph >> activateSubmenu: evt [
	"Activate our submenu; e.g., pass control to it"
	subMenu ifNil:[^false]. "not applicable"
	(subMenu fullContainsPoint: evt position) ifFalse:[^false].
	subMenu activate: evt.
	self removeAlarm: #deselectTimeOut:.
	^true
]

{ #category : #selecting }
MenuItemMorph >> adjacentTo [
	^ {self bounds topRight + (10 @ 0). self bounds topLeft}
]

{ #category : #accessing }
MenuItemMorph >> allWordingsNotInSubMenus: verbotenSubmenuContentsList [
	"Answer a collection of the wordings of all items and subitems, but omit the stay-up item, and also any items in any submenu whose tag is in verbotenSubmenuContentsList"

	self isStayUpItem ifTrue:[^ #()].
	subMenu ifNotNil:
		[^ (verbotenSubmenuContentsList includes: self contents asString)
			ifTrue:
				[#()]
			ifFalse:
				[subMenu allWordingsNotInSubMenus: verbotenSubmenuContentsList]].

	^ Array with: self contents asString
]

{ #category : #accessing }
MenuItemMorph >> arguments [

	^ arguments

]

{ #category : #accessing }
MenuItemMorph >> arguments: aCollection [

	arguments := aCollection.

]

{ #category : #private }
MenuItemMorph >> bottomArrow [
	^ ColorForm
		mappingWhiteToTransparentFrom: ((SubMenuMarker rotateBy: 90) asFormOfDepth:8)
]

{ #category : #accessing }
MenuItemMorph >> contentString [
	^self valueOfProperty: #contentString
]

{ #category : #accessing }
MenuItemMorph >> contentString: aString [ 
	aString ifNil: [self removeProperty: #contentString]
		ifNotNil: [self setProperty: #contentString toValue: aString]
]

{ #category : #accessing }
MenuItemMorph >> contents: aString [
	^self contents: aString withMarkers: true
]

{ #category : #accessing }
MenuItemMorph >> contents: aString withMarkers: aBool [
	^self contents: aString withMarkers: aBool inverse: false
]

{ #category : #accessing }
MenuItemMorph >> contents: aString withMarkers: aBool inverse: inverse [ 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	self contentString: nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
	self flag: #CleanYesNoOnOffAfter.
	(aString isKindOf: Association)
		ifTrue: [ super contents: aString value. 
				marker := aString key   
					ifTrue: [self onImage]
					ifFalse: [self offImage]]
		ifFalse: [
		(aString notEmpty and: [aString first = $<]) 
			ifFalse: [^super contents: aString].
		markerIndex := aString indexOf: $>.
		markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	self contentString: aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size)].
	"And set the marker"
	marker := ImageMorph new form: marker.
	marker position: self left @ (self top + 2).
	self addMorphFront: marker
]

{ #category : #initialization }
MenuItemMorph >> defaultBounds [
"answer the default bounds for the receiver"
	^ 0 @ 0 extent: 10 @ 10
]

{ #category : #initialization }
MenuItemMorph >> deleteIfPopUp: evt [
	"Recurse up for nested pop ups"
	owner ifNotNil:[owner deleteIfPopUp: evt].
]

{ #category : #selecting }
MenuItemMorph >> deselect: evt [
	self isSelected: false.
	subMenu ifNotNil: [
		owner ifNotNil:[owner activeSubmenu: nil].
		self removeAlarm: #deselectTimeOut:].
]

{ #category : #'meta actions' }
MenuItemMorph >> deselectItem [
	| item |
	self isSelected: false.
	subMenu ifNotNil: [subMenu deleteIfPopUp].
	(owner isMenuMorph) ifTrue:
		[item := owner popUpOwner.
		(item isMenuItemMorph) ifTrue: [item deselectItem]].

]

{ #category : #events }
MenuItemMorph >> deselectTimeOut: evt [
	"Deselect timout. Now really deselect"

	owner selectedItem == self
		ifTrue: [ 
			evt hand newMouseFocus: owner.
			owner selectItem: nil event: evt ]
]

{ #category : #events }
MenuItemMorph >> doButtonAction [
	"Called programattically, this should trigger the action for which the receiver is programmed"

	self invokeWithEvent: nil
]

{ #category : #drawing }
MenuItemMorph >> drawOn: aCanvas [ 
	| stringColor stringBounds |
	(isSelected and: [ isEnabled ])
		ifTrue: [aCanvas fillRectangle: self bounds fillStyle: self selectionFillStyle.
			stringColor := color negated]
		ifFalse: [stringColor := color].
	stringBounds := bounds.
	self hasIcon
		ifTrue: [| iconForm | 
			iconForm := self iconForm.
			aCanvas translucentImage: iconForm at: stringBounds left @ (self top + (self height - iconForm height // 2)).
			stringBounds := stringBounds left: stringBounds left + iconForm width + 2].
	self hasMarker
		ifTrue: [stringBounds := stringBounds left: stringBounds left + self submorphBounds width + 8].
	stringBounds := stringBounds top: stringBounds top + stringBounds bottom - self fontToUse height // 2.
	aCanvas
		drawString: self contents
		in: stringBounds
		font: self fontToUse
		color: stringColor.
	self hasSubMenu
		ifTrue: [| subMenuMarker subMenuMarkerPosition | 
			subMenuMarker := self subMenuMarker.
			subMenuMarkerPosition := self right - subMenuMarker width @ (self top + self bottom - subMenuMarker height // 2).
			aCanvas paintImage: subMenuMarker at: subMenuMarkerPosition]
]

{ #category : #grabbing }
MenuItemMorph >> duplicateMorph: evt [
	"Make and return a duplicate of the receiver's argument"
	| dup menu |
	dup := self duplicate isSelected: false.
	menu := UIManager default newMenuIn: self for: nil.
	menu addMorphFront: dup.
	menu bounds: self bounds.
	menu stayUp: true.
	evt hand grabMorph: menu from: owner. "duplicate was ownerless so use #grabMorph:from: here"
	^menu
]

{ #category : #accessing }
MenuItemMorph >> enabled [
	"Delegate to exisitng method."

	^self isEnabled
]

{ #category : #accessing }
MenuItemMorph >> enabled: aBoolean [
	"Delegate to exisitng method."

	self isEnabled: aBoolean
]

{ #category : #events }
MenuItemMorph >> handleMouseUp: anEvent [
	"The handling of control between menu item requires them to act on mouse up even if not the current focus. This is different from the default behavior which really only wants to handle mouse ups when they got mouse downs before"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	anEvent hand releaseMouseFocus: self.
	anEvent wasHandled: true.
	anEvent blueButtonChanged
		ifTrue:[self blueButtonUp: anEvent]
		ifFalse:[self mouseUp: anEvent].
]

{ #category : #events }
MenuItemMorph >> handlesMouseDown: evt [

	^ true

]

{ #category : #events }
MenuItemMorph >> handlesMouseOver: anEvent [
	^true
]

{ #category : #events }
MenuItemMorph >> handlesMouseOverDragging: evt [
	^true
]

{ #category : #accessing }
MenuItemMorph >> hasIcon [
	"Answer whether the receiver has an icon."
	^ icon notNil
]

{ #category : #accessing }
MenuItemMorph >> hasIconOrMarker [
	"Answer whether the receiver has an icon or a marker."
	^ self hasIcon or: [ self hasMarker ]
]

{ #category : #accessing }
MenuItemMorph >> hasMarker [
	"Answer whether the receiver has a marker morph."
	^self hasSubmorphs 
]

{ #category : #accessing }
MenuItemMorph >> hasSubMenu [
	"Return true if the receiver has a submenu"
	^subMenu notNil
]

{ #category : #accessing }
MenuItemMorph >> hasSubMenu: aMenuMorph [
	subMenu ifNil:[^false].
	subMenu == aMenuMorph ifTrue:[^true].
	^subMenu hasSubMenu: aMenuMorph
]

{ #category : #accessing }
MenuItemMorph >> icon [
	"answer the receiver's icon"
	^ icon
]

{ #category : #accessing }
MenuItemMorph >> icon: aForm [ 
	"change the the receiver's icon"
	icon := aForm.
	
]

{ #category : #private }
MenuItemMorph >> iconForm [
	"private - answer the form to be used as the icon"
	^ isEnabled
		ifTrue: [self icon]
		ifFalse: [self icon asGrayScale]
]

{ #category : #initialization }
MenuItemMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	""
	
	contents := ''.
	hasFocus := false.
	isEnabled := true.
	subMenu := nil.
	isSelected := false.
	target := nil.
	selector := nil.
	arguments := nil.
	font := StandardFonts menuFont.
	self hResizing: #spaceFill;
		 vResizing: #shrinkWrap
]

{ #category : #events }
MenuItemMorph >> invokeWithEvent: evt [
	"Perform the action associated with the given menu item."

	| w |
	self isEnabled ifFalse: [^ self].
	owner ifNotNil:[self isStayUpItem ifFalse:[
		self flag: #workAround. "The tile system invokes menus straightforwardly so the menu might not be in the world."
		(w := self world) ifNotNil:[
			owner deleteIfPopUp: evt.
			"Repair damage before invoking the action for better feedback"
			w displayWorldSafely]]].
	selector ifNil:[^self].
	Cursor normal showWhile: [ | selArgCount |  "show cursor in case item opens a new MVC window"
		(selArgCount := selector numArgs) = 0
			ifTrue:
				[target perform: selector]
			ifFalse:
				[selArgCount = arguments size
					ifTrue: [target perform: selector withArguments: arguments]
					ifFalse: [target perform: selector withArguments: (arguments copyWith: evt)]].
		self showShortcut.
		self changed].
]

{ #category : #accessing }
MenuItemMorph >> isEnabled [

	^ isEnabled

]

{ #category : #testing }
MenuItemMorph >> isEnabled: aBoolean [

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled := aBoolean.
	self color: (aBoolean ifTrue: [Color black] ifFalse: [Color gray]).

]

{ #category : #testing }
MenuItemMorph >> isMenuItemMorph [
	^ true
]

{ #category : #selecting }
MenuItemMorph >> isSelected [
	^ isSelected 
]

{ #category : #selecting }
MenuItemMorph >> isSelected: aBoolean [

	isSelected := aBoolean.
	self changed.

]

{ #category : #accessing }
MenuItemMorph >> isStayUpItem [

	^selector == #toggleStayUp:
]

{ #category : #private }
MenuItemMorph >> leftArrow [
	^ ColorForm
		mappingWhiteToTransparentFrom: ((SubMenuMarker rotateBy: 180)asFormOfDepth: 8)
]

{ #category : #layout }
MenuItemMorph >> minHeight [
	| iconHeight |
	iconHeight := self hasIcon
				ifTrue: [self icon height + 2]
				ifFalse: [0].
	^ self fontToUse height max: iconHeight 
]

{ #category : #layout }
MenuItemMorph >> minWidth [
	| subMenuWidth iconWidth markerWidth |
	subMenuWidth := self hasSubMenu
				ifTrue: [10]
				ifFalse: [0].
	iconWidth := self hasIcon
				ifTrue: [self icon width + 2]
				ifFalse: [0].
	markerWidth := self hasMarker
				ifTrue: [self submorphBounds width + 8]
				ifFalse: [0].
	^ (self fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth + 10
]

{ #category : #events }
MenuItemMorph >> mouseDown: evt [
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	evt shiftPressed ifTrue: [^ super mouseDown: evt].  "enable label editing"
	evt hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self event: evt. 

]

{ #category : #events }
MenuItemMorph >> mouseEnter: evt [
	"The mouse entered the receiver"

	owner ifNotNil: [owner stayUp ifFalse: [self mouseEnterDragging: evt]].
]

{ #category : #events }
MenuItemMorph >> mouseEnterDragging: evt [
	"The mouse entered the receiver. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	evt hand mouseFocus == owner ifTrue:[owner selectItem: self event: evt]
]

{ #category : #events }
MenuItemMorph >> mouseLeave: evt [
	"The mouse has left the interior of the receiver..."

	owner ifNotNil: [owner stayUp ifFalse: [self mouseLeaveDragging: evt]]
]

{ #category : #events }
MenuItemMorph >> mouseLeaveDragging: evt [ 
	"The mouse left the receiver. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."

	owner ifNil: [^self].
	evt hand mouseFocus == owner ifFalse: [^self].
	"If we have a submenu, make sure we've got some time to enter it before actually leaving the menu item"
	subMenu isNil 
		ifTrue: [owner selectItem: nil event: evt]
		ifFalse: 
			[self 
				addAlarm: #deselectTimeOut:
				with: evt
				after: 500]
]

{ #category : #events }
MenuItemMorph >> mouseUp: evt [
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	evt hand mouseFocus == owner ifFalse: [^self].
	self contentString ifNotNil:[
		self contents: self contentString withMarkers: true inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	self deselect: evt.
	self invokeWithEvent: evt.		

]

{ #category : #private }
MenuItemMorph >> offImage [
	"Return the form to be used for indicating an off marker"
	| form |
	form := Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	(form getCanvas)
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.9) 
			borderWidth: 1 borderColor: Color black.
	^form
]

{ #category : #private }
MenuItemMorph >> onImage [
	"Return the form to be used for indicating an on marker"
	| form |
	form := Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	(form getCanvas)
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 
			borderWidth: 1 borderColor: Color black;
		fillRectangle: (form boundingBox insetBy: 2) fillStyle: Color black.
	^form
]

{ #category : #private }
MenuItemMorph >> rightArrow [

	^ SubMenuMarker
]

{ #category : #selecting }
MenuItemMorph >> select: evt [
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: self adjacentTo
			forHand: evt hand
			from: self.
		subMenu selectItem: nil event: evt].
]

{ #category : #private }
MenuItemMorph >> selectionFillStyle [
	"answer the fill style to use with the receiver is the selected  
	element"
	Display depth <= 2 ifTrue: [^ Color gray].
	^ self theme settings menuSelectionColor.

]

{ #category : #accessing }
MenuItemMorph >> selector [

	^ selector

]

{ #category : #accessing }
MenuItemMorph >> selector: aSymbol [

	selector := aSymbol.

]

{ #category : #events }
MenuItemMorph >> showShortcut [

	ShortcutReminder showShortcut: self

	

]

{ #category : #accessing }
MenuItemMorph >> subMenu [

	^ subMenu

]

{ #category : #accessing }
MenuItemMorph >> subMenu: aMenuMorph [

	subMenu := aMenuMorph.
	self changed.

]

{ #category : #private }
MenuItemMorph >> subMenuMarker [
	"private - answer the form to be used as submenu marker"
	 ^ self rightArrow
]

{ #category : #accessing }
MenuItemMorph >> target [

	^ target
]

{ #category : #accessing }
MenuItemMorph >> target: anObject [

	target := anObject.

]

{ #category : #events }
MenuItemMorph >> themeChanged [
	"Also pass on to the submenu if any."

	super themeChanged.
	self subMenu ifNotNil:[ :m | m themeChanged ]
]

{ #category : #private }
MenuItemMorph >> upArrow [
	^ ColorForm
		mappingWhiteToTransparentFrom: ((SubMenuMarker rotateBy: 270)asFormOfDepth: 8)
]

{ #category : #copying }
MenuItemMorph >> veryDeepFixupWith: deepCopier [
	"If target and arguments fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

	super veryDeepFixupWith: deepCopier.
	target := deepCopier references at: target ifAbsent: [target].
	arguments notNil ifTrue:
	[arguments := arguments collect: [:each |
		deepCopier references at: each ifAbsent: [each]]]
]

{ #category : #copying }
MenuItemMorph >> veryDeepInner: deepCopier [ 
	"Copy all of my instance variables. Some need to be not copied  
	at all, but shared. Warning!! Every instance variable defined in  
	this class must be handled. We must also implement  
	veryDeepFixupWith:. See DeepCopier class comment."
	super veryDeepInner: deepCopier.
	isEnabled := isEnabled veryDeepCopyWith: deepCopier.
	subMenu := subMenu veryDeepCopyWith: deepCopier.
	isSelected := isSelected veryDeepCopyWith: deepCopier.
	icon := icon veryDeepCopyWith: deepCopier.
	"target := target.		Weakly copied"
	"selector := selector.		a Symbol"
	arguments := arguments
]

{ #category : #'meta actions' }
MenuItemMorph >> wantsHaloFromClick [
	"Only if I'm not a lonely submenu"
	^owner notNil and:[ owner hasSubmorphs ]
]
