Extension { #name : #Morph }

{ #category : #'*Polymorph-Widgets' }
Morph >> adoptPaneColor [
	"Adopt our pane color."

	self adoptPaneColor: self paneColor
]

{ #category : #'*Polymorph-Widgets' }
Morph >> boundsWithinCorners [
 	"Changed to be more realistic..."
	
	^self bounds insetBy: 2
]

{ #category : #'*Polymorph-Widgets' }
Morph >> drawKeyboardFocusOn: aCanvas [
	"Draw the keyboard focus indication."

	self focusIndicatorMorph
		drawOn: aCanvas
]

{ #category : #'*Polymorph-Widgets' }
Morph >> expandFullBoundsForDropShadow: aRectangle [
	"Return an expanded rectangle for an eventual drop shadow."
	
	^(aRectangle expandBy: self shadowMargins)
		quickMerge: aRectangle
]

{ #category : #'*Polymorph-Widgets' }
Morph >> focusBounds [
	"Answer the bounds for drawing the focus indication."

	^self bounds
]

{ #category : #'*Polymorph-Widgets' }
Morph >> focusChanged [
	"Report that the area occupied by the morph's focus indicator should be redrawn.
	Optimized for border-only (no fill)."
	
	|rects fm|
	fm := self focusIndicatorMorph.
	fm fillStyle isTransparent
		ifTrue: [fm borderWidth > 0 ifTrue: [
					rects := fm bounds areasOutside: (fm bounds insetBy: fm borderWidth).
					rects do: [:r | self invalidRect: r]]]
		ifFalse: [self invalidRect: fm bounds]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> focusColor [
	"Answer the keyboard focus indication color."

	^self borderStyle color contrastingBlackAndWhiteColor
]

{ #category : #'*Polymorph-Widgets' }
Morph >> focusIndicatorCornerRadius [
	"Answer the corner radius preferred for the focus indicator
	for the receiver for themes that support this."

	^self theme focusIndicatorCornerRadiusFor: self 
]

{ #category : #'*Polymorph-Widgets' }
Morph >> focusIndicatorMorph [
	"Answer the focus indicator morph for the receiver."

	^self theme focusIndicatorMorphFor: self
]

{ #category : #'*Polymorph-Widgets' }
Morph >> hasKeyboardFocus [
	"Answer whether the receiver has keyboard focus."

	^((self world ifNil: [^false]) 
		activeHand ifNil: [^false])  keyboardFocus = self
]

{ #category : #'*Polymorph-Widgets' }
Morph >> lastSubmorphRecursive [
	"Answer recursive last submorph of the receiver."

	^self hasSubmorphs
		ifTrue: [self lastSubmorph lastSubmorphRecursive]
		ifFalse: [self]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> layoutInBounds: cellBounds [
	"Layout specific. Apply the given bounds to the receiver after being layed out in its owner."
	| box aSymbol |
	
	cellBounds = self bounds ifTrue:[^self]. "already up to date. Fixed here to use bounds rather than fullBounds for the check."
	cellBounds extent = self bounds extent "nice fit. Fixed here to use bounds rather than fullBounds for the check."
		ifTrue:[^self position: cellBounds origin].
	box := bounds.
	self hResizing == #shrinkWrap
		ifTrue:[box := box origin extent: self minExtent x @ box height].
	self vResizing == #shrinkWrap
		ifTrue:[box := box origin extent: box width @ self minExtent y].
	"match #spaceFill constraints"
	self hResizing == #spaceFill 
		ifTrue:[box := box origin extent: cellBounds width @ box height].
	self vResizing == #spaceFill
		ifTrue:[box := box origin extent: box width @ cellBounds height].
	"align accordingly"
	aSymbol := (owner ifNil:[self]) cellPositioning.
	box := box align: (box perform: aSymbol) with: (cellBounds perform: aSymbol).
	"and install new bounds"
	self bounds: box.
]

{ #category : #'*Polymorph-Widgets' }
Morph >> minExtent [
	"Layout specific. Return the minimum size the receiver can be represented in.
	Implementation note: When this message is sent from an owner trying to lay out its children it will traverse down the morph tree and recompute the minimal arrangement of the morphs based on which the minimal extent is returned. When a morph with some layout strategy is encountered, the morph will ask its strategy to compute the new arrangement. However, since the final size given to the receiver is unknown at the point of the query, the assumption is made that the current bounds of the receiver are the base on which the layout should be computed. This scheme prevents strange layout changes when for instance, a table is contained in another table. Unless the inner table has been resized manually (which means its bounds are already enlarged) the arrangement of the inner table will not change here. Thus the entire layout computation is basically an iterative process which may have different results depending on the incremental changes applied.
	Fixed for shrinkWrap."

	| layout minExtent extra hFit vFit |
	hFit := self hResizing.
	vFit := self vResizing.
	(hFit == #rigid and: [vFit == #rigid]) 
		ifTrue: 
			["The receiver will not adjust to parents layout by growing or shrinking,
		which means that an accurate layout defines the minimum size."

			^self fullBounds extent max: self minWidth @ self minHeight].

	"An exception -- a receiver with #shrinkWrap constraints but no children is being treated #rigid (the equivalent to a #spaceFill receiver in a non-layouting owner)"
	self hasSubmorphs 
		ifFalse: 
			[hFit == #shrinkWrap ifTrue: [hFit := #rigid].
			vFit == #shrinkWrap ifTrue: [vFit := #rigid]].
	layout := self layoutPolicy.
	layout 
		ifNil: [minExtent := 0 @ 0]
		ifNotNil: [minExtent := layout minExtentOf: self in: self layoutBounds].
	hFit == #rigid 
		ifTrue: [minExtent := self fullBounds extent x @ minExtent y]
		ifFalse: 
			[extra := self bounds width - self layoutBounds width.
			minExtent := (minExtent x + extra) @ minExtent y].
	minExtent := vFit == #rigid 
				ifTrue: [minExtent x @ self fullBounds extent y]
				ifFalse: 
					[extra := self bounds height - self layoutBounds height.
					minExtent x @ (minExtent y + extra)].
	minExtent := minExtent max: self minWidth @ self minHeight.
	^minExtent
]

{ #category : #'*Polymorph-Widgets' }
Morph >> modalUnlockFrom: aSystemWindow [
	"Unlock the receiver as a modal owner of the given window."

	self unlock
]

{ #category : #'*Polymorph-Widgets' }
Morph >> myDependents [
	"Improved performance dependents."
	
	^ (self valueOfProperty: #myDependents) ifNil: [ #() ]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> myDependents: aCollectionOrNil [
	"Improved performance dependents."
	
	aCollectionOrNil 
		ifNil: [self removeProperty: #myDependents]
		ifNotNil: [self setProperty: #myDependents toValue: aCollectionOrNil]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> navigateFocusBackward [
	"Change the keyboard focus to the previous morph."

	self previousMorphWantingFocus ifNotNil: [:m |
		m takeKeyboardFocus]
	
]

{ #category : #'*Polymorph-Widgets' }
Morph >> navigateFocusForward [
	"Change the keyboard focus to the next morph."

	self nextMorphWantingFocus ifNotNil: [:m |
		m takeKeyboardFocus]
	
]

{ #category : #'*Polymorph-Widgets' }
Morph >> navigationKey: event [
	"Check for tab key activity and change focus as appropriate.
	Check for menu key to do popup."
	
	event isUserInterrupt ifTrue: [ ^ true ].

	(event keyCharacter = Character escape and: [
			event anyModifierKeyPressed]) ifTrue: [
		^ self yellowButtonActivity: false ].
	
	self window ifNotNil: [:win |
		(win handlesKeyStroke: event) ifTrue: [
			(win keyStroke: event) ifTrue: [^true]]].
	
	^false
]

{ #category : #'*Polymorph-Widgets' }
Morph >> optimalExtent [
	"Answer the submorphBounds extent plus twice our border width."
	
	^self submorphBounds extent + (self borderWidth * 2)
]

{ #category : #'*Polymorph-Widgets' }
Morph >> paneColor [
	"Answer the window's pane color or our color otherwise."

	^self paneColorOrNil ifNil: [self color]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> paneColor: aColor [
	"Explicitly set the pane color for the reveiver."

	self setProperty: #paneColor toValue: aColor.
	self adoptPaneColor
]

{ #category : #'*Polymorph-Widgets' }
Morph >> paneColorOrNil [
	"Answer the window's pane color or nil otherwise."

	^self valueOfProperty: #paneColor ifAbsent: [
		(self owner ifNil: [^nil]) paneColorOrNil]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> passivate [
	"Mark the receiver and submorphs as passive (background)."

	self submorphsDo: [:m | m passivate]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> preferredButtonCornerStyle [
	"Answer the preferred button corner style
	for submorphs. Answer nil for no preference."

	^nil
]

{ #category : #'*Polymorph-Widgets' }
Morph >> preferredCornerStyle [
	"Answer the preferred corner style."

	^#square
]

{ #category : #'*Polymorph-Widgets' }
Morph >> roundedCorners [
	"Return a list of those corners to round.

		1-4
		|  |
		2-3

	Returned array contains `codes' of those corners, which should be rounded.

	1 denotes top-left corner
	2 denotes bottom-left corner
	3 denotes bottom-right corner
	4 denotes top-right corner.

	Thus, if this method returned #(2 3) that would mean that bottom (left and right)
	corners would be rounded whereas top (left and right) corners wouldn't be rounded.

	This method returns #(1 2 3 4) and that means that all the corners should be rounded."

	^self valueOfProperty: #roundedCorners ifAbsent: [#(1 2 3 4)]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> roundedCorners: anArray [
	"Set the corners to round."

	anArray = #(1 2 3 4)
		ifTrue: [self removeProperty: #roundedCorners]
		ifFalse: [self setProperty: #roundedCorners toValue: anArray].
	self changed
]

{ #category : #'*Polymorph-Widgets' }
Morph >> showBalloon: msgString hand: aHand [
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	|w h|
	(w := self world) ifNil: [^self].
	h := aHand ifNil: [w activeHand].
	( self theme builder newBalloonHelp: msgString for: self balloonHelpAligner)
		popUpFor: self hand: h
]

{ #category : #'*Polymorph-Widgets-Themes' }
Morph class >> systemIconName [

	^ #morphIcon
]

{ #category : #'*Polymorph-Widgets' }
Morph >> tabKey: event [
	"Check for tab key activity and change focus as appropriate."

	event controlKeyPressed ifFalse: [
		event keyCharacter = Character tab ifTrue: [
			event shiftPressed
				ifTrue: [self navigateFocusBackward]
				ifFalse: [self navigateFocusForward].
			^true]].
	^false
]

{ #category : #'*Polymorph-Widgets' }
Morph >> takeKeyboardFocus [
	"Make the receiver the keyboard focus for the active hand."

	self activeHand newKeyboardFocus: self
]

{ #category : #'*Polymorph-Widgets' }
Morph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^ false
]

{ #category : #'*Polymorph-Widgets' }
Morph class >> theme [
	"Answer the ui theme that provides controls."

	^ Smalltalk ui theme
]

{ #category : #'*Polymorph-Widgets' }
Morph >> toggleVisible [
	"Toggle the visibility of the receiver."

	self visible
		ifTrue: [self hide]
		ifFalse: [self show]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> toggleVisibleAndRaise [
	"Toggle the visibility of the receiver, brining to
	the front if becoming visible."

	self visible
		ifTrue: [self hide]
		ifFalse: [self comeToFront; show]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> visible: aBoolean [ 
	"set the 'visible' attribute of the receiver to aBoolean.
	Must update owner layout since its full bounds may depend
	on the receiver extending beyond its bounds."
	
	(extension isNil and:[aBoolean]) ifTrue: [^ self].
	self visible == aBoolean ifTrue: [^ self].
	self assureExtension visible: aBoolean.
	self changed.
	owner ifNotNil: [owner layoutChanged]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> wantsKeyboardFocus [
	"Answer whether the receiver would like keyboard focus
	in the general case (mouse action normally)."

	^self takesKeyboardFocus and: [
		self visible and: [self enabled]]
]

{ #category : #'*Polymorph-Widgets' }
Morph >> wantsKeyboardFocusNavigation [
	"Answer whether the receiver would like keyboard focus
	when navigated to by keyboard."

	^self wantsKeyboardFocus
]
