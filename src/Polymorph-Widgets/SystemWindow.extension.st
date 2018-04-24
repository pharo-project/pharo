Extension { #name : #SystemWindow }

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> aboutText [
	"Answer the text to use for the About dialog."

	^self model
		ifNil: ['This is a system window without a model' translated]
		ifNotNil: [self model class instanceSide organization classComment
					ifEmpty: ['The model of this window has no class comment']
					ifNotEmpty: [:comment | comment]]
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> activeFillStyle [
	"Return the active fillStyle for the receiver."
	
	^self theme windowActiveFillStyleFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> activeLabelFillStyle [
	"Return the active label fillStyle for the receiver."
	
	^self theme windowActiveLabelFillStyleFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> activeTitleFillStyle [
	"Return the active title fillStyle for the receiver."
	
	^self theme windowActiveTitleFillStyleFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> addCollapseBox [
	"If I have a labelArea, add a collapse box to it."
	
	labelArea
		ifNil: [^ self].
	collapseBox := self createCollapseBox.
	collapseBox layoutFrame: (self theme windowCollapseBoxLayoutFor: self).
	labelArea addMorphBack: collapseBox
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> addExpandBox [
	"If I have a labelArea, add a close box to it"
	labelArea
		ifNil: [^ self].
	expandBox := self createExpandBox.
	self setExpandBoxBalloonText.
	expandBox layoutFrame: (LayoutFrame new leftFraction: 1; leftOffset: (self boxExtent x * 2 + 3) negated).
	labelArea addMorphBack: expandBox
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> addGrips [
	"Add the edge and corner grips."
	
	self
		addCornerGrips;
		addEdgeGrips
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> addGripsIfWanted [
	"Add the edge and corner grips if the window wants them."
	
	self wantsGrips ifTrue: [self addGrips]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> addMenuControl [
	"If I have a label area, add a menu control to it."

	labelArea ifNil: [^ self]. "No menu if no label area"
	menuBox
		ifNotNil: [menuBox delete].
	labelArea addMorphBack: (menuBox := self createMenuBox)
]

{ #category : #'*Polymorph-Widgets' }
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

	paneMorphs := paneMorphs copyReplaceFrom: 1 to: 0 with: (Array with: aMorph).
	"aMorph adoptPaneColor: self paneColor."
	aMorph
		borderStyle: (self theme windowPaneBorderStyleFor: aMorph in: self).
	self addMorphBack: aMorph. "reorder panes so flop-out right-side scrollbar is visible"
		
	self owner ifNotNil: [self addPaneSplitters] "do when opened for performance"
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> addPaneSplitters [
	"Add the vertical and horizontal pane splitters."
	
	self removePaneSplitters.
	self addPaneVSplitters.
	self addPaneHSplitters.
	self linkSplittersToSplitters
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> allowedToClose [
	"Answer whether the window is currently allowed to close."

	^self mustNotClose not and: [
		self modalChild isNil]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> animateClose [
	"Animate closing."
	
	| smallRect restoredRect rects steps|
	self isMinimized ifTrue: [^self].
	restoredRect := self bounds.
	smallRect := restoredRect scaledAndCenteredIn: (0@0 extent: 20@20).
	smallRect := smallRect align: smallRect center with: restoredRect center.
	steps := self theme settings animationSettings numberOfSteps.
	rects := ((steps - 1)/steps to: 0 by: -1/steps) collect: [:x |
		smallRect interpolateTo: restoredRect at: (20 raisedTo: x) - 1 / 19].
	World displayWorldSafely.
	self fastAnimateRectangles: rects
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> animateMaximize [
	"Animate maximizing from restored."
	
	|expandedRect restoredRect rects steps|
	expandedRect := self fullscreenBounds.
	restoredRect := self bounds.
	steps := self theme settings animationSettings numberOfSteps.
	rects := (1/steps to: 1 by: 1/steps) collect: [:x |
		restoredRect interpolateTo: expandedRect at: (20 raisedTo: x) - 1 / 19].
	self fastAnimateRectangles: rects
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> animateMinimize [
	"Animate minimizing."
	
	|tb buttonRect restoredRect rects steps|
	tb := self worldTaskbar ifNil: [^self].
	buttonRect := ((tb taskButtonOf: self) ifNil: [^self]) bounds.
	restoredRect := self isFlexed
		ifTrue: [(owner transform
					globalPointToLocal: fullFrame topLeft)
					extent: fullFrame extent]
		ifFalse: [fullFrame].
	steps := self theme settings animationSettings numberOfSteps.
	rects := ((steps - 1)/steps to: 0 by: -1/steps) collect: [:x |
		buttonRect interpolateTo: restoredRect at: (20 raisedTo: x) - 1 / 19].
	self fastAnimateRectangles: rects
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> animateRestore [
	"Animate restoring from maximised."
	
	|expandedRect restoredRect rects steps|
	expandedRect := self bounds.
	restoredRect := self unexpandedFrame.
	steps := self theme settings animationSettings numberOfSteps.
	rects := ((steps - 1)/steps to: 0 by: -1/steps) collect: [:x |
		restoredRect interpolateTo: expandedRect at: (20 raisedTo: x) - 1 / 19].
	self fastAnimateRectangles: rects
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> animateRestoreFromMinimized [
	"Animate restoring from minimised."
	
	|tb buttonRect restoredRect rects steps|
	tb := self worldTaskbar ifNil: [^self].
	buttonRect := ((tb taskButtonOf: self) ifNil: [^self]) bounds.
	restoredRect := self isFlexed
		ifTrue: [(owner transform
					globalPointToLocal: fullFrame topLeft)
					extent: fullFrame extent]
		ifFalse: [fullFrame].
	steps := self theme settings animationSettings numberOfSteps.
	rects := (1/steps to: 1 by: 1/steps) collect: [:x |
		buttonRect interpolateTo: restoredRect at: (20 raisedTo: x) - 1 / 19].
	self fastAnimateRectangles: rects
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> basicIsSticky [
	"Answer the super isSticky."

	^super isSticky
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> basicLabel [
	"Answer the actual label morph."

	^label
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> beWithGrips [
	"Add the grips and set a property to
	indicate that grips are wanted."

	self removeProperty: #noGrips.
	(self isCollapsed not or: [self isTaskbarPresent]) ifTrue: [
		self addGripsIfWanted]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> beWithoutGrips [
	"Remove the grips and set a property to
	indicate that grips are not wanted."

	self setProperty: #noGrips toValue: true.
	self removeGrips
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> bringBehind: aMorph [
	"Make the receiver be directly behind the given morph.
	Take into account any modal owner and propagate."

	|outerMorph|
	outerMorph := self topRendererOrSelf.
	outerMorph owner ifNil: [^ self "avoid spurious activate when drop in trash"].
	outerMorph owner addMorph: outerMorph after: aMorph topRendererOrSelf.
	self modalOwner ifNotNil: [:mo | mo bringBehind: self]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> buildWindowMenu [
  
	^ self menuBuilder menu.
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> closeBoxHit [
	"The user clicked on the close-box control in the window title.
	For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down.
	If we have a modal child then don't delete.
	Play the close sound now since this is the only time we know that the close is user-initiated."

	self allowedToClose ifFalse: [^self].
	self playCloseSound.
	self close

]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> collapseBoxHit [
	"The user has clicked on the collapse box.
	Collapse or expand the receiver as appropriate."
	
	self isCollapsed
		ifTrue: [self playRestoreUpSound]
		ifFalse: [self playMinimizeSound].
	self collapseOrExpand
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> configureForEmbedding [
	"Set up the window so it can be embedded into another morph."

	labelArea owner notNil ifTrue: [
		self
			hasDropShadow: false;
			beWithoutGrips;
			removeLabelArea;
			makeBorderless.
		self submorphsDo: [:m | m unlock]]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> configureForUnembedding [
	"Set up the window so it can be unembedded and placed in the world."

	labelArea owner isNil ifTrue: [
		self
			makeBordered;
			initializeLabelArea;
			themeChanged;
			addGrips]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> createCloseBox [
	"Answer a button for closing the window."
	
	^self theme createCloseBoxFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> createCollapseBox [
	"Answer a button for minimising the window."
	
	^self theme createCollapseBoxFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> createExpandBox [
	"Answer a button for maximising/restoring the window."
	
	^self theme createExpandBoxFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> createMenuBox [
	"Answer a button for the window menu."
	
	^self theme createMenuBoxFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> defaultBackgroundColor [
	"Answer the color to be used as the base window color."
	^self theme windowColorFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> doubleClick: event [
	"Handle a double click. Maximize/restore the window.
	Works in title bar area."

	self isResizeable ifFalse: [ ^ self ].
	
	(labelArea containsPoint: event position)
		ifTrue: [self expandBoxHit]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> doubleClickTimeout: event [
	"Forget the #inactiveDoubleClick property.
	The property is set if an inactive window was double-clicked."
	
 	self removeProperty: #inactiveDoubleClick
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> expandBoxHit [
	"The fullscreen expand box has been hit"

	self isCollapsed
		ifTrue: [self playRestoreUpSound.
				self
					hide;
					collapseOrExpand.
				self unexpandedFrame ifNil: [self unexpandedFrame: fullFrame].
				self
					fullscreen;
					setExpandBoxBalloonText.
				^self show].
	self unexpandedFrame
		ifNil: [self playMaximizeSound.
				self theme settings animationSettings useAnimation ifTrue: [self animateMaximize].
				self
					unexpandedFrame: fullFrame;
					fullscreen]
		ifNotNil: [self playRestoreDownSound.
				self theme settings animationSettings useAnimation ifTrue: [self animateRestore].
				self
					bounds: self unexpandedFrame;
					unexpandedFrame: nil].
	self setExpandBoxBalloonText
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> fastAnimateRectangles: rects [
	"Animate the given rectangles using the fast method."
	
	|delay col merged|
	rects ifEmpty: [^self].
	delay := Delay forMilliseconds: (self theme settings animationSettings delay).
	col := Color gray alpha: 0.5.
	merged := rects first.
	rects withIndexDo: [:r :i |
		Display
			border: r rounded
			width: 1
			rule: Form blend
			fillColor: col.
		merged := merged merge: r.
		delay wait].
	merged := merged expandBy: 1.
	World invalidRect: merged from: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> fillStyleToUse [
	"Answer the basic fill style for the receiver."

	^self isActive 
		ifTrue: [self activeFillStyle] 
		ifFalse: [self inactiveFillStyle]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> fullscreen [
	"Zoom Window to Full World size with possible DeskMargins"
	"SystemWindow fullscreen"

	self bounds: self fullscreenBounds
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> getRawLabel [
	"Answer a shallowCopy of the label with the contents fitted."
	
	|contentsFit|
	contentsFit := label shallowCopy fitContents.
	contentsFit extent: (label extent x min: contentsFit extent x) @ contentsFit extent y.
	^contentsFit
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> handlesDropShadowInHand [
	"Answer whether the receiver will handle drop shadow drawing when picked up in the hand."

	^self theme handlesWindowDropShadowInHandFor: self
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> hasCloseBox [
	"Answer whether the receiver currently has a close box."

	^closeBox notNil
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> hasCollapseBox [
	"Answer whether the receiver currently has a collapse box."

	^collapseBox notNil
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> hasExpandBox [
	"Answer whether the receiver currently has an expand box."

	^expandBox notNil
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> hasMenuBox [
	"Answer whether the receiver currently has a menu box."

	^menuBox notNil
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> inactiveFillStyle [
	"Return the active fillStyle for the receiver."
	
	^self theme windowInactiveFillStyleFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> inactiveLabelFillStyle [
	"Return the inactive label fillStyle for the receiver."
	
	^self theme windowInactiveLabelFillStyleFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> inactiveTitleFillStyle [
	"Return the inactive title fillStyle for the receiver."
	
	^self theme windowInactiveTitleFillStyleFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> indicateModalChild [
	"Make the user aware that this is the topmost modal child
	by flashing."

	(self isMinimized and: [self isTaskbarPresent])
		ifTrue: [self worldTaskbar ifNotNil: [:tb |
					tb indicateModalChildForMorph: self]]
		ifFalse: [self flash]
]

{ #category : #'*Polymorph-Widgets' }
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
	stripes := Array
						with: (Morph newBounds: bounds)
						with: (Morph newBounds: bounds).
	self addLabelArea.
	labelArea
		goBehind.
	self replaceBoxes.
	labelArea fillStyle: self activeTitleFillStyle.
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> isActive [
	"Answer active if no owner too to avoid color flickering."
	
	^self isWindowActive: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> isEmbedded [
	"Answer whether the receiver is embedded, i.e.
	has an owner that is not the world or the hand."

	^self owner isNotNil and: [self owner ~= self world and: [self owner ~= self activeHand]]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> isMaximized [
	"Answer whether we are maximised."

	^self unexpandedFrame notNil
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> isMinimized [
	"Answer whether we are minimised."

	^self isCollapsed
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> isNotMaximized [
	"Answer whether we are not maximised."

	^self unexpandedFrame isNil
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> isNotMinimized [
	"Answer whether we are not minimised."

	^self isCollapsed not
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> isNotRestored [
	"Answer whether we are maximised or minimised."

	^self isMinimized or: [self isMaximized]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> isRestored [
	"Answer whether we are neither expanded or collapsed."

	^(self isMinimized or: [self isMaximized]) not
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> keyStroke: evt [
	"Check for close window."
	super keyStroke: evt.
	(self navigationKey: evt) ifTrue: [^true].
	^false
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> labelArea [
	"Answer the label area."

	^labelArea
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> labelHeight [
	"Answer the height for the window label.  The standard behavior is at bottom; a hook is provided so that models can stipulate other heights, in support of various less-window-looking demos.
	If no label answer the class border width instead."

	label ifNil: [^self isEmbedded ifTrue: [0] ifFalse: [self class borderWidth]].
	^(label height + (self class borderWidth * 2)) max:
		(collapseBox ifNotNil: [collapseBox height] ifNil: [10])
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> labelString [
	"Answer the actual label string."

	^label isNil
		ifTrue: [labelString]
		ifFalse: [label contents asString]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> layoutChanged [
	"No need to propagate to the world.
	Fixed to always flush layout cache."

	(self owner isNil or: [self owner isWorldMorph not])
		ifTrue: [^super layoutChanged].
	fullBounds := nil.
	self layoutPolicy ifNotNil: [:layout | layout flushLayoutCache]
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> lockInactivePortions [
	"Make me unable to respond to mouse and keyboard.  Control boxes remain active."

	self isEmbedded ifTrue: [^self].
	self submorphsDo: [:m | m == labelArea ifFalse: [m lock]]
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> makeClosable [
	"Reinstate the close box. Go via theme to maintain box order."
	
	mustNotClose := false.
	closeBox
		ifNil: [closeBox := self createCloseBox.
				self replaceBoxes]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> maximize [
	"Maximise the receiver. If collapsed the uncollapse first."

	self isMinimized ifTrue: [self collapseOrExpand].
	self isMaximized ifFalse: [self expandBoxHit]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> menuBox [
	"Answer the receiver's menu box."

	^menuBox
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> minimize [
	"Minimise the receiver."

	self isMinimized ifFalse: [self collapseBoxHit]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> modalChild [
	"Answer the modal child of the receiver, if any."

	^self valueOfProperty: #modalChild
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> modalLockTo: aSystemWindow [
	"Lock the receiver as a modal owner of the given window."

	aSystemWindow
		setProperty: #modalOwner toValue: self.
	self setProperty: #modalChild toValue: aSystemWindow.
	
	closeBox ifNotNil: [ 
		self setProperty: #preModalCloseEnabled toValue: closeBox enabled.
		closeBox enabled: false
	].

]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> modalOwner [
	"Answer the modal owner of the receiver, if any."

	^self valueOfProperty: #modalOwner
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> modalUnlockFrom: aSystemWindow [
	"Unlock the receiver as a modal owner of the given window."

	aSystemWindow removeProperty: #modalOwner.
	self removeProperty: #modalChild.
	closeBox ifNotNil: [:cl | cl enabled: (self valueOfProperty: #preModalCloseEnabled ifAbsent: [true])].
	self removeProperty: #preModalCloseEnabled.
	self activate
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> model: anObject [
	"Set the model."
	
	super model: anObject.
	self paneColorTracksModel ifTrue: [
		self
			setProperty: #paneColor toValue: self defaultBackgroundColor;
			fillStyle: self fillStyleToUse;
			setStripeColorsFrom: self paneColorToUse.
		self theme settings fadedBackgroundWindows ifFalse: [ "since not done in stripes"
			self adoptPaneColor: self paneColor]].
	self minimumExtent: (
		(anObject respondsTo: #minimumExtent)
			ifTrue: [anObject minimumExtent]).
	menuBox ifNotNil: [
		menuBox
			labelGraphic: (self theme windowMenuIconFor: self);
			height: self boxExtent y]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> navigateFocusForward [
	"Change the keyboard focus to the next morph or the receiver
	in none are interested."

	self nextMorphWantingFocus
		ifNil: [self takeKeyboardFocus]
		ifNotNil: [super navigateFocusForward]
	
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> nextMorphAcrossInWindow [
	"Answer the next morph in the window. Traverse
	from the receiver to its next sibling or owner's next sibling etc.
	Make sure we behave like a normal Morph, if we aren't a toplevel window ( or not visible) "

	^ (self owner notNil and: [ self owner isWorldMorph ])
		ifTrue: [ self ]
		ifFalse: [ super nextMorphAcrossInWindow ]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> nextMorphInWindow [
	"Answer the next morph in the window. Traverse
	from the receiver to its first pane."

	^self hasSubmorphs
		ifTrue: [self submorphs first]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> openModal: aSystemWindow [
	"Open the given window locking the receiver until it is dismissed.
	Set the pane color to match the receiver.
	Answer the system window."
	
	aSystemWindow
		theme: self theme;
		setWindowColor: self paneColor.
	^super openModal: aSystemWindow
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> paneColor [

	"Answer the basic pane color that should be used."
	
	^self theme paneColorFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> paneColorOrNil [
	"Answer the window's pane color or nil otherwise."

	^self paneColor
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> paneColorTracksModel [
	"Answer true if the colour of the window should be taken from the model."
	
	^true
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow class >> passivateTopWindow [
	"Make no window the top window."

	TopWindow ifNotNil: [ :window | 
		TopWindow := nil.
		window privateBePassive.
	]

]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> playCloseSound [
	"Play the themed sound for closing."
	
	self theme windowCloseSound play
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> playMaximizeSound [
	"Play the themed sound for maximizing."
	
	self theme windowMaximizeSound play
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> playMinimizeSound [
	"Play the themed sound for maximizing."
	
	self theme windowMinimizeSound play
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> playOpenSound [
	"Play the themed sound for opening."
	
	self theme windowOpenSound play
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> playRestoreDownSound [
	"Play the themed sound for restoring from maximized."
	
	self theme windowRestoreDownSound play
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> playRestoreUpSound [
	"Play the themed sound for restoring from minimized."
	
	self theme windowRestoreUpSound play
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> preferredCornerStyle [
	"Answer the preferred corner style."

	^self theme windowPreferredCornerStyleFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> previousMorphInWindow [
	"Answer the previous morph in the window. This will be the
	last submorph recursively of the first pane morph."

	^self hasSubmorphs
		ifTrue: [self lastSubmorphRecursive]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> rememberKeyboardFocus: aMorph [
	"Record the current keyboard focus for the receiver."
	
	(aMorph isNil or: [(aMorph hasOwner: self) not]) ifFalse: [
		self setProperty: #rememberedFocus toValue: aMorph]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> rememberedKeyboardFocus [
	"Answer the remembered keyboard focus for the receiver."
	
	^self valueOfProperty: #rememberedFocus
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> removeBoxes [
	"Remove all label area boxes."

	closeBox ifNotNil: [closeBox delete. closeBox := nil].
	menuBox ifNotNil: [menuBox delete. menuBox := nil].
	expandBox ifNotNil: [expandBox delete. expandBox := nil].
	collapseBox ifNotNil: [collapseBox delete. collapseBox := nil]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> removeCloseBox [
	"Remove the close box."

	closeBox ifNotNil: [closeBox delete. closeBox := nil]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> removeCollapseBox [
	"Remove the collapse box."

	collapseBox ifNotNil: [collapseBox delete. collapseBox := nil]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> removeEdgeGrips [
	"Remove the window edge grips."
	
	|edges|
	edges := self submorphsSatisfying: [:each | each isKindOf: WindowEdgeGripMorph].
	edges do: [:each | each delete]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> removeExpandBox [
	"Remove the expand box."

	expandBox ifNotNil: [expandBox delete. expandBox := nil]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> removeGrips [
	"Remove the edge and corner grips."
	
	self
		removeCornerGrips;
		removeEdgeGrips
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> removeLabelArea [
	"Remove the entire label area."

	self removeGrips.
	labelArea delete.
	label := nil.	
	(self isCollapsed not or: [self isTaskbarPresent]) ifTrue: [
		self addGripsIfWanted]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> replaceBoxes [
	"Rebuild the various boxes."
	
	labelArea removeAllMorphs.
	self setLabelWidgetAllowance.
	self theme configureWindowLabelAreaFor: self.
	self setFramesForLabelArea.
	self isActive ifFalse: [labelArea passivate]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> resetCollapsedFrame [
	"Reset the collapsed frame."

	collapsedFrame := nil
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow class >> resetForms [
	"Set the box forms to nil so that they will be taken from the current theme.
	Don't touch the expand or menu ones, delegated to theme anyway."

	CloseBoxImage := nil.
	CollapseBoxImage := nil
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> restore [
	"Restore the receiver's normal size."

	self isMinimized
		ifTrue: [self collapseBoxHit]
		ifFalse: [self isMaximized ifTrue: [self expandBoxHit]]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> restoreAndActivate [
	"Restore the window if minimised then activate."

	self isMinimized
		ifTrue: [self restore].
	self isActive
		ifFalse: [self activate]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> setExpandBoxBalloonText [
	"Set the expand box balloon help text as appropriate."

	expandBox ifNil: [^self].
	self unexpandedFrame
		ifNil: [expandBox setBalloonText: 'Expand to full screen' translated]
		ifNotNil: [expandBox setBalloonText: 'Contract to original size' translated]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> setFramesForLabelArea [
	"Set the layout for the label area."
	
	self theme configureWindowLabelAreaFrameFor: self
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> setLabelWidgetAllowance [
	"Set the extra space required, in general, apart from the label.
	Should make theme based (for centered titles), leave enough room
	for the moment."
	
	^labelWidgetAllowance :=  (self boxExtent x * 7) 
	"allow for three on one side and centering plus a bit"
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> setWindowColor: incomingColor [
	"Removed existing color check - looked useless!"
	
	| aColor |
	incomingColor ifNil: [^ self].  "it happens"
	aColor := incomingColor.
	self setProperty: #paneColor toValue: aColor.
	self setStripeColorsFrom: aColor.
	self theme settings fadedBackgroundWindows 
		ifFalse: [self adoptPaneColor: aColor]. "reverse optimisation"
	self changed.
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> showAbout [

	self theme  
		longMessageIn: self
		text: self aboutText
		title: self aboutTitle
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> theme [
	"Answer the ui theme that provides controls.
	Don't call super since that implementation may delegate here."

	(self valueOfProperty: #theme) ifNotNil: [:t | ^ t].
	^self class theme
]

{ #category : #'*Polymorph-Widgets' }
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

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> toggleVisibleAndRaise [
	"Toggle the visibility of the receiver, bringing to
	the front if becoming visible. Activate or passivate here."

	self isActive ifTrue: [self world navigateVisibleWindowForward].
	super toggleVisibleAndRaise.
	self visible ifTrue: [self activate]
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> wantsGrips [
	"Answer whether the window wants edge and corner grips."
	
	^self isResizeable
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> wantsRoundedCorners [
	"Answer whether rounded corners are wanted."
	
	^(self theme windowPreferredCornerStyleFor: self) == #rounded
]

{ #category : #'*Polymorph-Widgets' }
SystemWindow >> window [
	"Answer the receiver's window."

	^self
]
