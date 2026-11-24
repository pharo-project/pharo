"
A morph whose submorphs comprise a paste-up of rectangular subparts which ""show through"".  Anything called a 'Playfield' is a PasteUpMorph.

Facilities commonly needed on pages of graphical presentations and on simulation playfields, such as the painting of new objects, turtle trails, gradient fills, background paintings, parts-bin behavior, collision-detection, etc., are (or will be) provided.
"
Class {
	#name : 'PasteUpMorph',
	#superclass : 'BorderedMorph',
	#instVars : [
		'backgroundMorph',
		'griddingOn'
	],
	#classVars : [
		'ShouldShowWorldMenu',
		'ShouldSwapMenuOpenerButtons',
		'WindowEventHandler',
		'WorldAnnouncer'
	],
	#category : 'Morphic-Core-Worlds',
	#package : 'Morphic-Core',
	#tag : 'Worlds'
}

{ #category : 'class initialization' }
PasteUpMorph class >> initialize [
	ShouldSwapMenuOpenerButtons := false.
	self shouldShowWorldMenu: true
]

{ #category : 'testing' }
PasteUpMorph class >> isMenuOpenByLeftClick [
	"Truth table:
	swap 	isMac	methodReturn
	0	 	0	 	1
	0	 	1	 	1
	1	 	0	 	0
	1	 	1 		1
"

	^ self shouldSwapMenuOpenerButtons ==> [ OSPlatform current isMacOS ]
]

{ #category : 'accessing' }
PasteUpMorph class >> shouldShowWorldMenu [
	^ ShouldShowWorldMenu
]

{ #category : 'accessing' }
PasteUpMorph class >> shouldShowWorldMenu: anObject [
	ShouldShowWorldMenu := anObject
]

{ #category : 'settings' }
PasteUpMorph class >> shouldShowWorldMenuSettingOn: aBuilder [

	<systemsettings>
	(aBuilder setting: #shouldShowWorldMenu)
		target: self;
		defaultValue: true;
		parentName: #morphic;
		label: 'Show world menu';
		description: 'Whether the World''s world menu should be shown or not on a click in the World.'
]

{ #category : 'accessing' }
PasteUpMorph class >> shouldSwapMenuOpenerButtons [
	^ ShouldSwapMenuOpenerButtons
]

{ #category : 'accessing' }
PasteUpMorph class >> shouldSwapMenuOpenerButtons: aBoolean [
	^ ShouldSwapMenuOpenerButtons := aBoolean
]

{ #category : 'settings' }
PasteUpMorph class >> shouldSwapMenuOpenerButtonsSetting: aBuilder [

	<systemsettings>
	(aBuilder setting: #shouldSwapMenuOpenerButtons)
		label: 'Invert right and left click on world menu in Linux and Windows';
		parentName: #desktopSettings;
		target: self;
		getSelector: #shouldSwapMenuOpenerButtons;
		setSelector: #shouldSwapMenuOpenerButtons:;
		description:
			'If checked the behaviour of the world menu is changed only under Linux and Windows. The main menu will appear on right click instead of left click.';
		defaultValue: false
]

{ #category : 'event handling' }
PasteUpMorph >> acceptDroppingMorph: dropped event: evt [
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	| aMorph |
	aMorph := self morphToDropFrom: dropped.
	super acceptDroppingMorph: aMorph event: evt.
	aMorph
		submorphsDo: [ :m |
			m isHaloMorph
				ifTrue: [ m delete ] ].
	self bringTopmostsToFront
]

{ #category : 'world state' }
PasteUpMorph >> addMorph: aMorph centeredNear: aPoint [
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect := Rectangle center: aPoint extent: aMorph fullBounds extent.
	delta := trialRect amountToTranslateWithin: bounds.
	aMorph position: trialRect origin + delta.
	self addMorph: aMorph
]

{ #category : 'submorphs - add/remove' }
PasteUpMorph >> addMorphFront: aMorph [

	^self addMorphInFrontOfLayer: aMorph
]

{ #category : 'wiw support' }
PasteUpMorph >> addMorphInLayer: aMorph [
	super addMorphInLayer: aMorph.
	aMorph wantsToBeTopmost ifFalse:[self bringTopmostsToFront]
]

{ #category : 'accessing' }
PasteUpMorph >> backgroundImage: aForm layout: aSymbol [
	"Set a background image, replacing the current background morph, if any,
	with the given layout (see AlphaImageMorph layoutSymbols). "

	self backgroundMorph: (AlphaImageMorph new
			 hResizing: #rigid;
			 vResizing: #rigid;
			 image: aForm;
			 autoSize: false;
			 layout: aSymbol;
			 lock)
]

{ #category : 'accessing' }
PasteUpMorph >> backgroundMorph [
	"Answer the background morph if any."

	^backgroundMorph
]

{ #category : 'accessing' }
PasteUpMorph >> backgroundMorph: aMorph [

	"Set the background morph.
	Probably best if locked prior to adding."

	self backgroundMorph ifNotNil: [ self backgroundMorph delete ].
	backgroundMorph := aMorph.
	aMorph
		ifNotNil: [ aMorph bounds: self bounds.
			self addMorphBack: aMorph
			]
]

{ #category : 'viewing' }
PasteUpMorph >> bringTopmostsToFront [
	submorphs
		select:[:m| m wantsToBeTopmost]
		thenDo:[:m| self addMorphInLayer: m]
]

{ #category : 'session management' }
PasteUpMorph >> checkSession [

	"do nothing"
]

{ #category : 'stepping' }
PasteUpMorph >> cleanseOtherworldlySteppers [
	"If the current project is a morphic one, then remove from its steplist
	those morphs that are not really in the world"
	"Utilities cleanseOtherworldlySteppers"
	| old delta |
	old := self world stepListSize.
	self world steppingMorphsNotInWorld
		do: [:m | m delete].
	self world cleanseStepList.
	(delta := old - self world stepListSize) > 0
		ifTrue: [ self newLineTrace: delta asString , ' morphs removed from steplist' ]
]

{ #category : 'windows' }
PasteUpMorph >> closeAllUnchangedWindows [

	(ConfirmationRequest signal: 'Do you really want to close all unchanged windows ?') ifFalse: [ ^ self ].
	(self windowsSatisfying: [ :w | w model canDiscardEdits ]) do: [ :w | w delete ]
]

{ #category : 'windows' }
PasteUpMorph >> closeUnchangedWindows [
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."

	(ConfirmationRequest signal: 'Do you really want to close all windows
except those with unaccepted edits?') ifFalse: [ ^ self ].

	self closeAllUnchangedWindows
]

{ #category : 'world menu' }
PasteUpMorph >> collapseNonWindows [
	self nonWindows reject: [:m | m isSticky] thenDo: [:m | m collapse]
]

{ #category : 'accessing' }
PasteUpMorph >> currentWindow [

	"Answer the top window or nil"

	^ self class environment at: #SystemWindow ifPresent: [:systemWindow | systemWindow topWindow]
]

{ #category : 'initialization' }
PasteUpMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ Color
		r: 0.861
		g: 1.0
		b: 0.722
]

{ #category : 'initialization' }
PasteUpMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 1
]

{ #category : 'initialization' }
PasteUpMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Color
		r: 0.8
		g: 1.0
		b: 0.6
]

{ #category : 'menu & halo' }
PasteUpMorph >> dispatchKeystroke: anEvent [
	anEvent keyCharacter == Character tab
		ifTrue: [
			self tabAmongFields
				ifTrue: [ ^ self tabHitWithEvent: anEvent ] ]
]

{ #category : 'drawing' }
PasteUpMorph >> drawBackgroundSketchOn: aCanvas [
	backgroundMorph ifNil: [ ^ self ].
	self clipSubmorphs
		ifTrue: [ aCanvas clipBy: self clippingBounds during: [ :canvas | canvas fullDrawMorph: backgroundMorph ] ]
		ifFalse: [ aCanvas fullDrawMorph: backgroundMorph ]
]

{ #category : 'drawing' }
PasteUpMorph >> drawGridOn: aCanvas [

	(self griddingOn and: [ self gridVisible ])
		ifTrue: [
			aCanvas
				fillRectangle: self bounds
				fillStyle:
					(self
						gridFormOrigin: self gridOrigin
						grid: self gridModulus
						background: nil
						line: Color lightGray) ]
]

{ #category : 'drawing' }
PasteUpMorph >> drawOn: aCanvas [
	"Draw in order:
	- background color
	- grid, if any
	- background sketch, if any
	Later (in drawSubmorphsOn:) I will skip drawing the background sketch."

	super drawOn: aCanvas.
	self drawGridOn: aCanvas.
	self drawBackgroundSketchOn: aCanvas
]

{ #category : 'drawing' }
PasteUpMorph >> drawSubmorphsOn: aCanvas [
	"Display submorphs back to front, but skip my background sketch."

	| drawBlock |
	submorphs isEmpty
		ifTrue: [ ^ self ].
	drawBlock := [ :canvas |
	submorphs
		reverseDo: [ :m |
			m ~~ backgroundMorph
				ifTrue: [ canvas fullDrawMorph: m ] ] ].
	self clipSubmorphs
		ifTrue: [ aCanvas clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock ]
		ifFalse: [ drawBlock value: aCanvas ]
]

{ #category : 'dropping/grabbing' }
PasteUpMorph >> dropEnabled [
	"Get this morph's ability to add and remove morphs via drag-n-drop."

	^ (self valueOfProperty: #dropEnabled) ~~ false
]

{ #category : 'event handling' }
PasteUpMorph >> dropFiles: anEvent [
	"Handle a number of dropped files from the OS.
	TODO:
		- use a more general mechanism for figuring out what to do with the file (perhaps even offering a choice from a menu)
		- remember the resource location or (when in browser) even the actual file handle"

	| numFiles |
	numFiles := anEvent contents.
	1 to: numFiles do: [ :i |
		| aFileReference handler |
		aFileReference := anEvent requestDropReference: i.
		handler := ExternalDropHandler
			lookupExternalDropHandler: aFileReference.
		handler
			ifNotNil: [ handler handle: aFileReference in: self dropEvent: anEvent ] ]
]

{ #category : 'drawing' }
PasteUpMorph >> gridFormOrigin: origin grid: smallGrid background: backColor line: lineColor [

	| bigGrid gridForm gridOrigin |
	gridOrigin := origin \\ smallGrid.
	bigGrid := (smallGrid asPoint x) @ (smallGrid asPoint y).
	gridForm := Form extent: bigGrid depth: self currentWorld worldState worldRenderer depth.
	backColor ifNotNil: [gridForm fillWithColor: backColor].
	gridOrigin x to: gridForm width by: smallGrid x do:
		[:x | gridForm fill: (x@0 extent: 1@gridForm height) fillColor: lineColor].
	gridOrigin y to: gridForm height by: smallGrid y do:
		[:y | gridForm fill: (0@y extent: gridForm width@1) fillColor: lineColor].
	^ InfiniteForm with: gridForm
]

{ #category : 'gridding' }
PasteUpMorph >> gridModulus [

	^ self gridSpec extent
]

{ #category : 'gridding' }
PasteUpMorph >> gridModulus: newModulus [

	self gridSpecPut: (self gridOrigin extent: newModulus).
	self changed
]

{ #category : 'gridding' }
PasteUpMorph >> gridOrigin [

	^ self gridSpec origin
]

{ #category : 'gridding' }
PasteUpMorph >> gridOrigin: newOrigin [

	^ self gridSpecPut: (newOrigin extent: self gridModulus)
]

{ #category : 'gridding' }
PasteUpMorph >> gridSpec [
	"Gridding rectangle provides origin and modulus"

	^ self valueOfProperty: #gridSpec ifAbsent: [0@0 extent: 8@8]
]

{ #category : 'gridding' }
PasteUpMorph >> gridSpecPut: newSpec [
	"Gridding rectangle provides origin and modulus"

	^ self setProperty: #gridSpec toValue: newSpec
]

{ #category : 'gridding' }
PasteUpMorph >> gridVisible [

	^ self hasProperty: #gridVisible
]

{ #category : 'gridding' }
PasteUpMorph >> gridVisibleOnOff [

	self setProperty: #gridVisible toValue: self gridVisible not.
	self changed
]

{ #category : 'gridding' }
PasteUpMorph >> gridVisibleString [
	"Answer a string to be used in a menu offering the opportunity
	to show or hide the grid"
	^ (self gridVisible
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'show grid when gridding' translated
]

{ #category : 'gridding' }
PasteUpMorph >> griddingOn [

	^ griddingOn ifNil: [false]
]

{ #category : 'gridding' }
PasteUpMorph >> griddingOnOff [

	griddingOn := self griddingOn not.
	self changed
]

{ #category : 'gridding' }
PasteUpMorph >> griddingString [
	"Answer a string to use in a menu offering the user the
	opportunity to start or stop using gridding"
	^ (self griddingOn
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'use gridding' translated
]

{ #category : 'event handling' }
PasteUpMorph >> handlesKeyboard: evt [
	^ true "handle all not handled events"
]

{ #category : 'event handling' }
PasteUpMorph >> handlesMouseDown: evt [
	^true
]

{ #category : 'thumbnail' }
PasteUpMorph >> icon [
	^ self iconNamed: #home
]

{ #category : 'initialization' }
PasteUpMorph >> initialize [
	"initialize the state of the receiver"

	super initialize.
	self enableDragNDrop.
	self clipSubmorphs: true
]

{ #category : 'cursor' }
PasteUpMorph >> isCursorOwner [
	^ false
]

{ #category : 'testing' }
PasteUpMorph >> isEasySelecting [
	"This is to isolate easySelection predicate.
	Selectors in holders make no sense so we are limiting easy
	selection to the worldMorph.
	It would also make sense in playfield so feel free to adjust this
	predicate. Selection can always be forced by using the shift
	before mouse down."
	^ false
]

{ #category : 'event handling' }
PasteUpMorph >> keyStroke: anEvent [
	"A keystroke has been made.  Service event handlers and, if it's a keystroke presented to the world, dispatch it to #unfocusedKeystroke:"

	super keyStroke: anEvent. "Give event handlers a chance"
	self dispatchKeystroke: anEvent
]

{ #category : 'world menu' }
PasteUpMorph >> keyboardNavigationHandler [
	"Answer the receiver's existing keyboardNavigationHandler, or nil if none."

	| aHandler |
	aHandler := self valueOfProperty: #keyboardNavigationHandler ifAbsent: [^ nil].
	(aHandler hasProperty: #moribund) ifTrue:  "got clobbered in another project"
		[self removeProperty: #keyboardNavigationHandler.
		^ nil].
	^ aHandler
]

{ #category : 'world menu' }
PasteUpMorph >> keyboardNavigationHandler: aHandler [
	"Set the receiver's keyboard navigation handler as indicated.  A nil argument means to remove the handler"

	aHandler
		ifNil:
			[self removeProperty: #keyboardNavigationHandler]
		ifNotNil:
			[self setProperty: #keyboardNavigationHandler toValue: aHandler]
]

{ #category : 'world menu' }
PasteUpMorph >> lastKeystroke [
	"Answer the last keystroke fielded by the receiver"
	^ self valueOfProperty: #lastKeystroke ifAbsent: ['']
]

{ #category : 'world menu' }
PasteUpMorph >> lastKeystroke: aString [
	"Remember the last keystroke fielded by the receiver"
	^ self setProperty: #lastKeystroke toValue: aString
]

{ #category : 'accessing' }
PasteUpMorph >> modalWindow: aMorph [

	(self valueOfProperty: #modalWindow) ifNotNil: [ :morph | morph delete ].
	self setProperty: #modalWindow toValue: aMorph
]

{ #category : 'dropping/grabbing' }
PasteUpMorph >> morphToDropFrom: aMorph [
	"Given a morph being carried by the hand, which the hand is about to drop, answer the actual morph to be deposited.  Normally this would be just the morph itself, but several unusual cases arise, which this method is designed to service."

	^aMorph
]

{ #category : 'event handling' }
PasteUpMorph >> morphToGrab: event [
	"Return the morph to grab from a mouse down event. If none, return nil."
	self submorphsDo:[:m|
		((m rejectsEvent: event) not and:[m fullContainsPoint: event cursorPoint]) ifTrue:[^m].
	].
	^nil
]

{ #category : 'submorphs - accessing' }
PasteUpMorph >> morphsInFrontOf: someMorph overlapping: aRectangle do: aBlock [
	"Include hands if the receiver is the World"
	self handsDo:[:m|
		m == someMorph ifTrue:["Try getting out quickly"
			owner ifNil:[^self].
			^owner morphsInFrontOf: self overlapping: aRectangle do: aBlock].
		"The hand only overlaps if it's not the hardware cursor"
		m needsToBeDrawn ifTrue:[
			(m fullBoundsInWorld intersects: aRectangle)
				ifTrue:[aBlock value: m]]].
	^super morphsInFrontOf: someMorph overlapping: aRectangle do: aBlock
]

{ #category : 'dropping/grabbing' }
PasteUpMorph >> mouseDown: evt [
	"Handle a mouse down event."

	| grabbedMorph |
	grabbedMorph := self morphToGrab: evt.
	grabbedMorph ifNotNil: [
		grabbedMorph isSticky ifTrue: [ ^ self ].
		^ evt hand grabMorph: grabbedMorph ].
	(super handlesMouseDown: evt) ifTrue: [ ^ super mouseDown: evt ].
	evt hand releaseKeyboardFocus. "shake of keyboard foci"
	"Menus and selections on my background are done by the desktop, from a root event handler"
	evt wasHandled: false
]

{ #category : 'world menu' }
PasteUpMorph >> nonWindows [
	^ (self submorphs select: [:m | (m isSystemWindow) not and: [m wantsToBeTopmost not]])
]

{ #category : 'accessing' }
PasteUpMorph >> osWindow [

	^ nil
]

{ #category : 'dropping/grabbing' }
PasteUpMorph >> positionNear: aPoint forExtent: anExtent adjustmentSuggestion: adjustmentPoint [
	"Compute a plausible positioning for adding a subpart of size anExtent, somewhere near aPoint, using adjustmentPoint as the unit of adjustment"

	| adjustedPosition |
	adjustedPosition := aPoint.
	[((self morphsAt: (adjustedPosition + (anExtent // 2))) size > 1) and:  "that 1 is self here"
		[bounds containsPoint: adjustedPosition]]
	whileTrue:
		[adjustedPosition := adjustedPosition + adjustmentPoint].

	^ adjustedPosition
]

{ #category : 'caching' }
PasteUpMorph >> releaseCachedState [
	super releaseCachedState.
	self removeModalWindow
]

{ #category : 'accessing' }
PasteUpMorph >> removeModalWindow [
	self modalWindow: nil
]

{ #category : 'dropping/grabbing' }
PasteUpMorph >> repelsMorph: aMorph event: ev [
	(aMorph wantsToBeDroppedInto: self) ifFalse: [^ false].
	self dropEnabled ifFalse: [^ true].
	(self wantsDroppedMorph: aMorph event: ev) ifFalse: [^ true].
	^ super repelsMorph: aMorph event: ev "consults #repelling flag"
]

{ #category : 'recategorized' }
PasteUpMorph >> resizeBackgroundMorph [

	"Resize the background morph to fit the world."

	self backgroundMorph ifNotNil: [ self backgroundMorph extent: self extent ]
]

{ #category : 'wiw support' }
PasteUpMorph >> shouldGetStepsFrom: aWorld [

	(self isWorldMorph and: [ owner isNotNil ]) ifTrue: [
		^ self outermostWorldMorph == aWorld ].
	^ super shouldGetStepsFrom: aWorld
]

{ #category : 'project state' }
PasteUpMorph >> viewBox: newViewBox [
	"I am now displayed within newViewBox; react."

	super position: newViewBox topLeft.
	fullBounds := bounds := newViewBox
]

{ #category : 'dropping/grabbing' }
PasteUpMorph >> wantsDroppedMorph: aMorph event: evt [

	self visible ifFalse: [^ false].  "will be a call to #hidden again very soon"
	self dropEnabled ifFalse: [^ false].
	^ true
]

{ #category : 'halos and balloon help' }
PasteUpMorph >> wantsHaloFromClick [
	 ^owner isSystemWindow not
]

{ #category : 'event handling' }
PasteUpMorph >> wantsKeyboardFocusFor: aSubmorph [

	aSubmorph wouldAcceptKeyboardFocus ifTrue: [ ^ true].
	^ super wantsKeyboardFocusFor: aSubmorph
]

{ #category : 'event handling' }
PasteUpMorph >> wantsWindowEvent: anEvent [

	^ self windowEventHandler isNotNil
]

{ #category : 'event handling' }
PasteUpMorph >> windowEvent: anEvent [
	self windowEventHandler ifNotNil: [ ^ self windowEventHandler windowEvent: anEvent ]
]

{ #category : 'event handling' }
PasteUpMorph >> windowEventHandler [
	"This is a class variable so it is global to all projects and does not get saved"
	^WindowEventHandler
]

{ #category : 'event handling' }
PasteUpMorph >> windowEventHandler: anObject [
	"This is a class variable so it is global to all projects and does not get saved"
	WindowEventHandler := anObject
]

{ #category : 'accessing' }
PasteUpMorph >> windowsSatisfying: windowBlock [

	| windows |
	windows := OrderedCollection new.
	self submorphs do: [:m |
		m embeddedWindowOrNil ifNotNil: [
			(windowBlock value: m)
				ifTrue: [ windows addLast: m ] ] ].

	^ windows
]
