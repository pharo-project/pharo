"
A DockingBarToggleMenuItemMorph is a specialized version of its superclass for DockingBar behavior.

There is a bit of duplication between 
DockingBarToggleMenuItemMorph and DockingBarMenuItemMorph because iof single inheritance limit: 

DockingBarToggleMenuItemMorph inherits from ToggleMenuItemMorph (and also from DockingBarMenuItemMorph).

One of the problem is that in addition, the use of toggleMenuItem or simple menuItem looks random.

DockingBarMenuItemMorph is created by
	add: wordingString icon: aForm help: helpString subMenu: aMenuMorph 
	
and DockingBarToggleMenuItemMorph is created by
	add: wordingString font: aFont icon: aForm help: helpString subMenu: aMenuMorph 

"
Class {
	#name : #DockingBarToggleMenuItemMorph,
	#superclass : #ToggleMenuItemMorph,
	#category : #'Morphic-Widgets-Extra'
}

{ #category : #selecting }
DockingBarToggleMenuItemMorph >> adjacentTo [
	"Adjusted to line up more nicely."
	self owner isFloating
		ifTrue: [^ {self bounds bottomLeft + (3 @ 3).  self bounds bottomRight + (5 @ 3)}].
	self owner isAdheringToTop
		ifTrue: [^ {self bounds bottomLeft + (5 @ 5)}].
	self owner isAdheringToLeft
		ifTrue: [^ {self bounds topRight + (5 @ 5)}].
	self owner isAdheringToBottom
		ifTrue: [^ {self bounds topLeft + (5 @ 5)}].
	self owner isAdheringToRight
		ifTrue: [^ {self bounds topLeft + (5 @ -5)}].
	^ {self bounds bottomLeft + (3 @ 5)}
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> deselectTimeOut: evt [
	"Deselect timout. Now really deselect"

	owner selectedItem == self
		ifTrue: [ 
			evt hand releaseMouseFocus: owner.
			owner selectItem: nil event: evt ]
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> drawOn: aCanvas [ 
	| stringColor stringBounds |
	(isSelected and: [ isEnabled ])
		ifTrue: [aCanvas fillRectangle: self bounds fillStyle: self selectionFillStyle.
			stringColor := color negated]
		ifFalse: [stringColor := color].
	stringBounds := bounds.
	stringBounds := stringBounds left: stringBounds left + 4.
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
			subMenuMarkerPosition := subMenuMarkerPosition - (4 @ -1).
			aCanvas paintImage: subMenuMarker at: subMenuMarkerPosition]
]

{ #category : #'drawing-private' }
DockingBarToggleMenuItemMorph >> drawSubMenuMarker: aForm on: aCanvas in: aRectangle [
	"Draw the submenu marker on the canvas within the given bounds."
	
	|markerRect| 
	markerRect := aRectangle topRight + (aForm width negated @ (aRectangle height - aForm height // 2)) extent: aForm extent.
	markerRect translateBy: -4@1.
	self drawIcon: aForm on: aCanvas in: markerRect
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> mouseDown: evt [
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	(evt shiftPressed and: [self isEditable ]) 
		ifTrue: [ ^ super mouseDown: evt ].  "enable label editing"

	isSelected
		ifTrue: [
			evt hand newMouseFocus: nil.
			owner selectItem: nil event: evt. ]
		ifFalse: [
			evt hand newMouseFocus: owner. "Redirect to menu for valid transitions"
			owner selectItem: self event: evt. ]

]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> mouseEnter: evt [
	"The mouse entered the receiver"

	owner ifNotNil: [owner stayUp ifFalse: [self mouseEnterDragging: evt]].
	(owner selectedItem notNil
				and: [owner selectedItem ~~ self])
			ifTrue: [owner selectItem: self event: evt.].


]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> mouseUp: evt [
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	evt hand mouseFocus == owner ifFalse: [^self].
	
	self contentString ifNotNil:[
		self contents: self contentString withMarkers: true inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	owner rootMenu selectItem: nil event: evt.
	self invokeWithEvent: evt.		

]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> noteNewOwner: aMorph [ 
	"I have just been added as a submorph of aMorph"
	super noteNewOwner: aMorph.

	self updateLayoutInDockingBar
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> ownerChanged [
	"The receiver's owner, some kind of a pasteup, has changed its 
	layout."
	super ownerChanged.
	self updateLayoutInDockingBar
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> roundedCorners [
	"Return a list of those corners to round"
	owner isFloating
		ifTrue: [^ #(1 4 )].
	owner isAdheringToTop
		ifTrue: [^ #(1 4 )].
	owner isAdheringToBottom
		ifTrue: [^ #(2 3 )].
	owner isAdheringToLeft
		ifTrue: [^ #(1 2 )].
	owner isAdheringToRight
		ifTrue: [^ #(3 4 )].
	^ #(1 2 3 4 )
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> selectionBorderStyle [
	"Answer the border style to use with the receiver is the selected element."
	
	^ self theme menuItemInDockingBarSelectedBorderStyleFor: self
	
]

{ #category : #private }
DockingBarToggleMenuItemMorph >> selectionFillStyle [
	"Answer the fill style to use with the receiver is the selected element."
	
	^self theme menuItemInDockingBarSelectedFillStyleFor: self
		
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> stringBoundsToUse [
	"Answer the bounds to use when drawing the item text."
	
	^self bounds left: self left + 4
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> subMenuMarker [
	"private - answer the form to be used as submenu marker"

	owner isFloating
		ifTrue: [ ^ self bottomArrow ].
	owner isAdheringToTop
		ifTrue: [ ^ self bottomArrow ].
	owner isAdheringToBottom
		ifTrue: [ ^ self upArrow ].
	owner isAdheringToLeft
		ifTrue: [ ^ self rightArrow ].
	owner isAdheringToRight
		ifTrue: [ ^ self leftArrow ].
	^ self rightArrow
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> updateLayoutInDockingBar [

	owner isVertical
		ifTrue: [
			self hResizing: #spaceFill.
			self vResizing: #shrinkWrap]
		ifFalse: [
			self hResizing: #shrinkWrap.
			self vResizing: #spaceFill].
	self extent: self minWidth @ self minHeight
]

{ #category : #accessing }
DockingBarToggleMenuItemMorph >> wantsRoundedCorners [
	^self theme currentSettings preferRoundCorner
		
]
