"
I'm a kind of container which adhere to one edge of the screen. See me in action with: 

DockingBarMorph new 
	addMorph: (SimpleButtonMorph new
                                           label: 'Say hello';
                                           target: [UIManager inform: 'Hello'];
                                           actionSelector: #value);
	addMorph: (SimpleButtonMorph new
                                           label: 'Say bonjour';
                                           target: [UIManager inform: 'Bonjour'];
                                           actionSelector: #value);
	addMorph: (SimpleButtonMorph new
                                           label: 'Close';
                                           target: [DockingBarMorph allInstances last delete];
                                           actionSelector: #value);
	adhereToBottom;
	openInWorld.
"
Class {
	#name : #DockingBarMorph,
	#superclass : #AlignmentMorph,
	#instVars : [
		'originalColor',
		'gradientRamp',
		'fillsOwner',
		'avoidVisibleBordersAtEdge',
		'autoGradient',
		'selectedItem',
		'activeSubMenu'
	],
	#category : #'Morphic-Widgets-Extra'
}

{ #category : #example }
DockingBarMorph class >> example [
	"self example"
	<exampleWidget>
	
	DockingBarMorph new 
		addMorph: (SimpleButtonMorph new
                                           label: 'Say hello';
                                           target: [UIManager inform: 'Hello'];
                                           actionSelector: #value);
		addMorph: (SimpleButtonMorph new
                                           label: 'Say bonjour';
                                           target: [UIManager inform: 'Bonjour'];
                                           actionSelector: #value);
		addMorph: (SimpleButtonMorph new
                                           label: 'Close';
                                           target: [DockingBarMorph allInstances last delete];
                                           actionSelector: #value);
	adhereToBottom;
	openInWorld.
]

{ #category : #example }
DockingBarMorph class >> exampleWithMenu [
	"self exampleWithMenu"
	<exampleWidget>
	
	DockingBarMorph new 
		add: 'Foo' 
		font:  StandardFonts defaultFont 
		icon: MenuMorph pushPinImage
		help: 'No idea' subMenu: nil;
		add: 'Bar' 
		font:  StandardFonts defaultFont 
		icon: MenuMorph pushPinImage
		help: 'No idea' subMenu: nil;
	adhereToTop;
	openInWorld.
]

{ #category : #'dropping/grabbing' }
DockingBarMorph >> aboutToBeGrabbedBy: aHand [ 
	"The morph is about to be grabbed, make it float"
	self beFloating.
	self updateBounds.
	self updateColor.
	(self bounds containsPoint: aHand position)
		ifFalse: [self center: aHand position].

]

{ #category : #'events-processing' }
DockingBarMorph >> activate: evt [ 
	"Receiver should be activated; e.g., so that control passes  
	correctly."
	evt hand newMouseFocus: self.
	self takeKeyboardFocus
]

{ #category : #control }
DockingBarMorph >> activeSubmenu: aSubmenu [

	activeSubMenu ifNotNil: [ activeSubMenu delete ].
	activeSubMenu := aSubmenu.
	aSubmenu ifNil: [ ^ self ].
	activeSubMenu selectItem: nil event: nil.
	activeSubMenu borderColor: self borderColor.
	activeSubMenu beSticky.
	activeSubMenu resistsRemoval: true.
	activeSubMenu removeMatchString
]

{ #category : #menu }
DockingBarMorph >> add: wordingString font: aFont icon: aForm help: helpString subMenu: aMenuMorph [

	"Append the given submenu with the given label."

	| item |

	item := DockingBarToggleMenuItemMorph new.
	item
		font: aFont;
		contents: wordingString;
		subMenu: aMenuMorph;
		icon: aForm.
	helpString ifNotNil: [ item setBalloonText: helpString ].
	self addMorphBack: item
]

{ #category : #construction }
DockingBarMorph >> add: wordingString icon: aForm help: helpString action: anAction keyText: aText [
	"Append the given submenu with the given label."

	| item |
	item := self newMenuItem.
	item contents: wordingString.
	item keyText: aText.
	anAction
		ifNotNil: [ :act | 
			item
				target: act receiver;
				selector: act selector;
				arguments: act arguments ].
	item icon: aForm.
	helpString ifNotNil: [ item setBalloonText: helpString ].
	self addMorphBack: item
]

{ #category : #construction }
DockingBarMorph >> add: wordingString icon: aForm help: helpString subMenu: aMenuMorph [
	"Append the given submenu with the given label."
	
	self add: wordingString icon: aForm help: helpString subMenu: aMenuMorph action: nil keyText: nil
]

{ #category : #construction }
DockingBarMorph >> add: wordingString icon: aForm help: helpString subMenu: aMenuMorph action: anAction keyText: aText [
	"Append the given submenu with the given label."

	| item |
	item := self newMenuItem
		contents: wordingString;
		subMenu: aMenuMorph;
		icon: aForm;
		keyText: aText;
		yourself.
	anAction
		ifNotNil: [ :act | 
			item
				target: act receiver;
				selector: act selector;
				arguments: act arguments ].
	helpString ifNotNil: [ item setBalloonText: helpString ].
	self addMorphBack: item
]

{ #category : #construction }
DockingBarMorph >> add: wordingString icon: aForm subMenu: aMenuMorph [ 
	"Append the given submenu with the given label."

	^ self add: wordingString icon: aForm help: nil subMenu: aMenuMorph 
]

{ #category : #construction }
DockingBarMorph >> add: aString subMenu: aMenuMorph [ 
	"Append the given submenu with the given label."
	self add: aString icon: nil subMenu: aMenuMorph 
]

{ #category : #accessing }
DockingBarMorph >> addBlankIconsIfNecessary: anIcon [ 
	"If any of my items have an icon, ensure that all do by using 
	anIcon for those that don't"
	self items
		reject: [:each | each hasIconOrMarker]
		thenDo: [:each | each icon: anIcon]
]

{ #category : #menu }
DockingBarMorph >> addCustomMenuItems: aMenu hand: aHandMorph [ 
	"Populate aMenu with appropriate menu items for a  
	yellow-button (context menu) click."
	super addCustomMenuItems: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu addUpdating: #autoGradientString selector: #toggleAutoGradient.
	self isFloating
		ifFalse: [
			aMenu addUpdating: #fillsOwnerString selector: #toggleFillsOwner.
			aMenu addUpdating: #avoidVisibleBordersAtEdgeString selector: #toggleAvoidVisibleBordersAtEdge]
]

{ #category : #construction }
DockingBarMorph >> addLine [
	"Append a divider line to this menu. Suppress duplicate lines."

	submorphs isEmpty ifTrue: [^ self].
	(self lastSubmorph isKindOf: DockingBarMenuLineMorph)
		ifFalse: [self addMorphBack: DockingBarMenuLineMorph new].

]

{ #category : #construction }
DockingBarMorph >> addSpace: sizePointOrNumber [ 
	"Add a new space of the given size to the receiver."
	| space |
	space := Morph new.
	space extent: sizePointOrNumber asPoint.
	space color: Color transparent.
	space borderWidth: 0.
	self addMorphBack: space
]

{ #category : #construction }
DockingBarMorph >> addSpacer [
	"Add a new spacer to the receiver. 
	 
	Spacer are objects that try to use as much space as they can"
	self
		addMorphBack: (AlignmentMorph newSpacer: Color transparent)
]

{ #category : #'private - accessing' }
DockingBarMorph >> adhereTo: edgeSymbol [ 
	"Private - Instruct the receiver to adhere to the given edge.  
	 
	Options: #left #top #right #bottom or #none"
	(#(#left #top #right #bottom #none ) includes: edgeSymbol)
		ifFalse: [^ self error: 'invalid option'].
	self setToAdhereToEdge: edgeSymbol.
	self updateLayoutProperties.
	self updateColor
]

{ #category : #accessing }
DockingBarMorph >> adhereToBottom [
	"Instract the receiver to adhere to bottom"
	 self adhereTo:#bottom
]

{ #category : #accessing }
DockingBarMorph >> adhereToLeft [
	"Instract the receiver to adhere to left"
	self adhereTo: #left
]

{ #category : #accessing }
DockingBarMorph >> adhereToRight [
	"Instract the receiver to adhere to right"
	self adhereTo: #right
]

{ #category : #accessing }
DockingBarMorph >> adhereToTop [
	"Instract the receiver to adhere to top"
	self adhereTo: #top
]

{ #category : #accessing }
DockingBarMorph >> adoptPaneColor: paneColor [
	"Change our color too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	originalColor :=  paneColor.
	self borderStyle baseColor: paneColor.
	self updateColor
]

{ #category : #accessing }
DockingBarMorph >> autoGradient [
	"Answer if the receiver is in autoGradient mode"
	^ autoGradient
]

{ #category : #accessing }
DockingBarMorph >> autoGradient: aBoolean [ 
	"Instruct the receiver to fill the owner or not"
	autoGradient := aBoolean.
	self updateColor
]

{ #category : #menu }
DockingBarMorph >> autoGradientString [
	"Answer the string to be shown in a menu to represent the  'resistsRemoval' status"
	
	^ (self autoGradient) ->  'auto gradient' translated
]

{ #category : #accessing }
DockingBarMorph >> avoidVisibleBordersAtEdge [
"Answer if the receiver is in avoidVisibleBordersAtEdge mode"
	^ avoidVisibleBordersAtEdge
]

{ #category : #accessing }
DockingBarMorph >> avoidVisibleBordersAtEdge: aBoolean [ 
	"Instruct the receiver to avoid showing the borders at edge"
	avoidVisibleBordersAtEdge := aBoolean.
self updateLayoutProperties.
]

{ #category : #menu }
DockingBarMorph >> avoidVisibleBordersAtEdgeString [
	"Answer the string to be shown in a menu to represent the visible status"
	^ (self avoidVisibleBordersAtEdge) ->  'avoid visible borders at edge' translated
]

{ #category : #accessing }
DockingBarMorph >> beFloating [
	"Instract the receiver to be floating"
	self adhereTo: #none
]

{ #category : #accessing }
DockingBarMorph >> color: aColor [ 
	"Set the receiver's color."
	super color: aColor.
	originalColor := aColor asColor.
	self updateColor
]

{ #category : #initialization }
DockingBarMorph >> defaultBorderColor [
	^ self theme menuBorderColor
]

{ #category : #initialization }
DockingBarMorph >> defaultBorderWidth [
	^ self theme dockingBarBorderWidth
]

{ #category : #initialization }
DockingBarMorph >> defaultColor [
	^ self theme settings derivedMenuColor
]

{ #category : #'submorphs-add/remove' }
DockingBarMorph >> delete [
	activeSubMenu
		ifNotNil: [activeSubMenu delete].
	^ super delete
]

{ #category : #control }
DockingBarMorph >> deleteIfPopUp: evt [ 
	evt
		ifNotNil: [evt hand releaseMouseFocus: self]
]

{ #category : #'change reporting' }
DockingBarMorph >> displayExtentChanged [

	self updateBounds
]

{ #category : #'private - accessing' }
DockingBarMorph >> edgeToAdhereTo [
	"private - answer the edge where the receiver is adhering to"
	^ self
		valueOfProperty: #edgeToAdhereTo
		ifAbsent: [#none]
]

{ #category : #accessing }
DockingBarMorph >> extent: aPoint [ 
	"Change the receiver's extent.
	optimized to not keep updating the (gradient) color!"
	
	|old|
	old := self extent.
	super extent: aPoint.
	self extent = old ifTrue: [^self].
	self updateColor
]

{ #category : #accessing }
DockingBarMorph >> fillsOwner [
	"Answer if the receiver is in fillOwner mode"
	^ fillsOwner
]

{ #category : #accessing }
DockingBarMorph >> fillsOwner: aBoolean [ 
	"Instruct the receiver to fill the owner or not"
	fillsOwner := aBoolean.
self updateLayoutProperties
]

{ #category : #menu }
DockingBarMorph >> fillsOwnerString [
	"Answer the string to be shown in a menu to represent the fills owner status"
	^ (self fillsOwner) -> 'fills owner' translated

]

{ #category : #'private - layout' }
DockingBarMorph >> gradientRamp [
	^ gradientRamp ifNil:[{0.0 -> originalColor muchLighter. 1.0 -> originalColor twiceDarker}]
]

{ #category : #'private - layout' }
DockingBarMorph >> gradientRamp: colorRamp [ 
	gradientRamp := colorRamp.
	self updateColor
]

{ #category : #'events-processing' }
DockingBarMorph >> handleFocusEvent: evt [
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	(evt isMouse and: [ evt isMouseUp ]) ifTrue: [ ^ self mouseUp: evt ].

	self processEvent: evt.

	"Need to handle keyboard input if we have the focus."
	evt isKeyboard ifTrue: [^ self handleEvent: evt].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(evt isMouseOver or:[evt isMouse not]) ifTrue:[^self].
	"What remains are mouse buttons and moves"
	evt isMove ifFalse:[^self handleEvent: evt]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: evt) ifTrue:[^self]].

]

{ #category : #'events-processing' }
DockingBarMorph >> handlesKeyboard: evt [
	^ true
]

{ #category : #'events-processing' }
DockingBarMorph >> handlesMouseDown: evt [
	^ true
]

{ #category : #initialization }
DockingBarMorph >> initialize [
	"initialize the receiver"

	super initialize.
	""
	fillsOwner := true.
	avoidVisibleBordersAtEdge := true.
	autoGradient := self wantsGradientByDefault.
	self color: self defaultColor.	"<= This is done in the super initialize but we need to use the setter in this class"
	""
	self beFloating.
	""
	self layoutInset: 0
]

{ #category : #testing }
DockingBarMorph >> isAdheringToBottom [
	"Answer true if the receiver is adhering to bottom"
	^ self edgeToAdhereTo == #bottom
]

{ #category : #testing }
DockingBarMorph >> isAdheringToLeft [
	"Answer true if the receiver is adhering to left"
	^ self edgeToAdhereTo == #left
]

{ #category : #testing }
DockingBarMorph >> isAdheringToRight [
	"Answer true if the receiver is adhering to right"
	^ self edgeToAdhereTo == #right
]

{ #category : #testing }
DockingBarMorph >> isAdheringToTop [
	"Answer true if the receiver is adhering to top"
	^ self edgeToAdhereTo == #top
]

{ #category : #testing }
DockingBarMorph >> isDockingBar [
	"Return true if the receiver is a docking bar"
	^ true
]

{ #category : #testing }
DockingBarMorph >> isFloating [
	"Answer true if the receiver has a float layout"
	^ self isHorizontal not
		and: [self isVertical not]
]

{ #category : #testing }
DockingBarMorph >> isHorizontal [
	"Answer true if the receiver has a horizontal layout"
	^ self isAdheringToTop
		or: [self isAdheringToBottom]
]

{ #category : #testing }
DockingBarMorph >> isVertical [
	"Answer true if the receiver has a vertical layout"
	^ self isAdheringToLeft
		or: [self isAdheringToRight]

]

{ #category : #'dropping/grabbing' }
DockingBarMorph >> justDroppedInto: aMorph event: anEvent [

	| ownerBounds leftRegion droppedPosition rightRegion topRegion bottomRegion |

	super justDroppedInto: aMorph event: anEvent.

	self owner ifNil: [ ^ self ].
	ownerBounds := aMorph bounds.
	topRegion := ownerBounds bottom: ownerBounds top + ( ownerBounds height // 5 ).
	bottomRegion := ownerBounds top: ownerBounds bottom - ( ownerBounds height // 5 ).

	leftRegion := ownerBounds right: ownerBounds left + ( ownerBounds width // 5 ).
	leftRegion := leftRegion top: topRegion bottom.
	leftRegion := leftRegion bottom: bottomRegion top.
	rightRegion := ownerBounds left: ownerBounds right - ( ownerBounds width // 5 ).
	rightRegion := rightRegion top: topRegion bottom.
	rightRegion := rightRegion bottom: bottomRegion top.

	droppedPosition := anEvent position.
	( topRegion containsPoint: droppedPosition )
		ifTrue: [ ^ self adhereToTop ].
	( bottomRegion containsPoint: droppedPosition )
		ifTrue: [ ^ self adhereToBottom ].
	( leftRegion containsPoint: droppedPosition )
		ifTrue: [ ^ self adhereToLeft ].
	( rightRegion containsPoint: droppedPosition )
		ifTrue: [ ^ self adhereToRight ].
	self beFloating
]

{ #category : #'events-processing' }
DockingBarMorph >> keyStroke: anEvent [
	"Handle menu navigation"
	
	"Left arrow key"
	anEvent keyValue = 28 ifTrue: [ self moveSelectionRight: -1 event: anEvent ].
	"Right arrow key"
	anEvent keyValue = 29 ifTrue: [ self moveSelectionRight: 1 event: anEvent ].
	"Bottom key"
	anEvent keyValue = 31 ifTrue: [ (selectedItem ifNotNil: [ selectedItem subMenu ifNotNil: [ :subMenu | subMenu activateFromKeyboard: anEvent ] ] )].
	^ super keyStroke: anEvent
]

{ #category : #'wiw support' }
DockingBarMorph >> morphicLayerNumber [
	"helpful for insuring some morphs always appear in front of or 
	behind others. smaller numbers are in front"
	^ 11
]

{ #category : #'events-processing' }
DockingBarMorph >> mouseDown: anEvent [
	"Release the mouse focus if clicked outside the receiver."

	(self fullContainsPoint: anEvent position)
		ifFalse: [anEvent hand releaseMouseFocus: self].
	^super mouseDown: anEvent
]

{ #category : #'events-processing' }
DockingBarMorph >> mouseUp: evt [
	"If we click on a selected item with an action we want to execute the action and deselect the menu item."
	
	super mouseUp: evt.
	
	self selectedItem ifNil: [ ^ self ]. "No selected item"
	self selectedItem selector ifNil: [ ^ self ]. "No action for the selected item"
	(self selectedItem containsPoint: evt cursorPoint) ifFalse: [ ^ self ]. "The user did not clicked on the selected menu item"

	"Execute the action of the menu item and deselect it"
	self selectedItem invokeWithEvent: evt.
	self selectItem: nil event: evt
]

{ #category : #'events-processing' }
DockingBarMorph >> moveSelectionRight: direction event: anEvent [
	"Move the current selection left or right by one, presumably under keyboard control.
	direction = +/-1"

	| index |
	index := (submorphs indexOf: selectedItem ifAbsent: [ 1 - direction ]) + direction.
	submorphs
		do: [ :unused | 
			"Ensure finite"
			| m |
			m := submorphs atWrap: index.
			(m isMenuItemMorph and: [ m isEnabled ])
				ifTrue: [ ^ self selectItem: m event: anEvent ].
			"Keep looking for an enabled item"
			index := index + direction sign ].
	^ self selectItem: nil event: anEvent
]

{ #category : #construction }
DockingBarMorph >> newMenuItem [
	
	^ DockingBarMenuItemMorph new
]

{ #category : #'submorphs-accessing' }
DockingBarMorph >> noteNewOwner: aMorph [ 
	"I have just been added as a submorph of aMorph"
	super noteNewOwner: aMorph.

	self submorphs
		do: [:each | each adjustLayoutBounds].

]

{ #category : #accessing }
DockingBarMorph >> originalColor [
	"Answer the original color."
	
	^originalColor
]

{ #category : #'change reporting' }
DockingBarMorph >> ownerChanged [
"The receiver's owner has changed its layout. "
	self updateBounds.
	^ super ownerChanged
]

{ #category : #accessing }
DockingBarMorph >> popUpOwner [
	"For compatibility with other menu"
	
	^ nil
]

{ #category : #'private - accessing' }
DockingBarMorph >> predominantDockingBarsOfChastes: predominantChastes [ 
	"Private - Answer a collection of the docking bar of my owner  
	that are predominant to the receiver.  
	 
	By 'predominant' we mean docking bar that have the right to  
	get a position before the receiver.  
	 
	The predominance of individual living in the same chaste is  
	determinated by the arrival order. "

	| allDockingBars byChaste byArrival |
	(self owner isNil or: [self owner isHandMorph]) ifTrue: [^ #()].

	allDockingBars := self owner dockingBars.

	byChaste := allDockingBars select: [:each | predominantChastes includes: each edgeToAdhereTo].
	(predominantChastes includes: self edgeToAdhereTo) ifFalse: [^ byChaste].
	byChaste := byChaste reject: [:each | each edgeToAdhereTo = self edgeToAdhereTo].	
	byArrival := allDockingBars select: [:each | each edgeToAdhereTo = self edgeToAdhereTo].
	byArrival := byArrival copyAfter: self.
	^ byChaste , byArrival
]

{ #category : #accessing }
DockingBarMorph >> rootMenu [
	^ self
]

{ #category : #rounding }
DockingBarMorph >> roundedCorners [
	"Return a list of those corners to round"
	self isAdheringToTop
		ifTrue: [^ #(2 3 )].
	self isAdheringToBottom
		ifTrue: [^ #(1 4 )].
	self isAdheringToLeft 
		ifTrue: [^ #(3 4 )].
	self isAdheringToRight
		ifTrue: [^ #(1 2 )].
	^ #(1 2 3 4 )
]

{ #category : #control }
DockingBarMorph >> selectItem: aMenuItem event: anEvent [ 
	selectedItem
		ifNotNil: [selectedItem deselect: anEvent].
	selectedItem := aMenuItem.
	selectedItem
		ifNotNil: [selectedItem select: anEvent]
]

{ #category : #private }
DockingBarMorph >> selectedItem [

	selectedItem ifNil: [ ^ nil ].
	^ selectedItem isSelected
		ifTrue: [ selectedItem ]
		ifFalse: [ nil ]
]

{ #category : #construction }
DockingBarMorph >> snapToEdgeIfAppropriate [
	(self owner isNil
			or: [self owner isHandMorph])
		ifTrue: [^ self].
	self updateBounds
]

{ #category : #testing }
DockingBarMorph >> stayUp [
	^ false
]

{ #category : #menu }
DockingBarMorph >> toggleAutoGradient [
	self autoGradient: self autoGradient not
]

{ #category : #menu }
DockingBarMorph >> toggleAvoidVisibleBordersAtEdge [
	self avoidVisibleBordersAtEdge: self avoidVisibleBordersAtEdge not
]

{ #category : #menu }
DockingBarMorph >> toggleFillsOwner [
	self fillsOwner: self fillsOwner not
]

{ #category : #'private - layout' }
DockingBarMorph >> updateBounds [
	"private - update the receiver's bounds"
	self updateExtent.
	self isFloating
		ifFalse: [self updatePosition]
]

{ #category : #'private - layout' }
DockingBarMorph >> updateColor [
	"private - update the receiver's color"
	| fill |
	self autoGradient
		ifFalse: [^ self].
	""
	fill := GradientFillStyle ramp: self gradientRamp.
	""
	fill origin: self topLeft.
	self isVertical
		ifTrue: [fill direction: self width @ 0]
		ifFalse: [fill direction: 0 @ self height].
	""
	self fillStyle: fill
]

{ #category : #'private - layout' }
DockingBarMorph >> updateExtent [
	"private - update the receiver's extent"

	| margin usedHeight |
	self fullBounds.
	self fillsOwner ifFalse: [ ^ self ].
	
	"Nil check because there is probably a race condition here making the CI fail a lot."
	self owner
		ifNotNil: [ :anOwner | 
			margin := self avoidVisibleBordersAtEdge ifTrue: [ self borderWidth * 2 ] ifFalse: [ 0 ].
			self isHorizontal ifTrue: [ self width: anOwner width + margin ].
			self isVertical
				ifTrue: [ usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#top #bottom).
					self height: anOwner height + margin - usedHeight ] ]
]

{ #category : #'private - layout' }
DockingBarMorph >> updateLayoutProperties [
	"private - update the layout properties based on adhering,  
	fillsOwner and avoidVisibleBordersAtEdge preferencs"

	(self isHorizontal or: [self isFloating])
		ifTrue: [self listDirection: #leftToRight]
		ifFalse: [self listDirection: #topToBottom].

	self hResizing: #shrinkWrap.
	self vResizing: #shrinkWrap.
	self fillsOwner ifTrue: [
			self isHorizontal ifTrue: [self hResizing: #spaceFill].
			self isVertical ifTrue: [self vResizing: #spaceFill]].
	
]

{ #category : #'private - layout' }
DockingBarMorph >> updatePosition [
	"private - update the receiver's position.
	Fixed so as not to keep changing position!
	(called twice if adhereing)"

	self owner
		ifNotNil: [ :anOwner | 
			| edgeSymbol margin usedHeight |
			edgeSymbol := self edgeToAdhereTo.
			edgeSymbol == #none
				ifTrue: [ self perform: (edgeSymbol , ':') asSymbol with: (anOwner perform: edgeSymbol) ].

			margin := (self avoidVisibleBordersAtEdge ifTrue: [ self borderWidth ] ifFalse: [ 0 ]) asPoint.

			self isAdheringToTop
				ifTrue: [ usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#top).
					self topLeft: anOwner topLeft - margin + (0 @ usedHeight) ].

			self isAdheringToBottom
				ifTrue: [ usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#bottom).
					self bottomLeft: anOwner bottomLeft + (-1 @ 1 * margin) - (0 @ usedHeight) ].

			self isAdheringToLeft
				ifTrue: [ | usedWidth |
					usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#top).
					usedWidth := self usedWidthByPredominantDockingBarsOfChastes: #(#left).
					self topLeft: anOwner topLeft - margin + (usedWidth @ usedHeight) ].

			self isAdheringToRight
				ifTrue: [ | usedWidth |
					usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#top).
					usedWidth := self usedWidthByPredominantDockingBarsOfChastes: #(#right).
					self topRight: anOwner topRight + (1 @ -1 * margin) + (usedWidth negated @ usedHeight) ] ]
]

{ #category : #'private - accessing' }
DockingBarMorph >> usedHeightByPredominantDockingBarsOfChastes: predominantChastes [ 
	"Private - convenience"
	^(self predominantDockingBarsOfChastes: predominantChastes)
		ifEmpty: [0]
		ifNotEmpty: [:predominants | (predominants collect: [:each | each height]) sum] 
]

{ #category : #'private - accessing' }
DockingBarMorph >> usedWidthByPredominantDockingBarsOfChastes: predominantChastes [ 
	"Private - convenience"
	^(self predominantDockingBarsOfChastes: predominantChastes)
		ifEmpty: [0]
		ifNotEmpty: [:predominants | (predominants collect: [:each | each width]) sum]
]

{ #category : #initialization }
DockingBarMorph >> wantsGradientByDefault [
	^ self theme preferGradientFill
]

{ #category : #testing }
DockingBarMorph >> wantsToBeTopmost [
	"Answer if the receiver want to be one of the topmost objects in 
	its owner"
	^ true
]

{ #category : #menu }
DockingBarMorph >> wantsYellowButtonMenu [
	"Answer true if the receiver wants a yellow button menu.
	Fixed for when defaultYellowButtonMenuEnabled setting is off"
	
	^ self defaultYellowButtonMenuEnabled
]
