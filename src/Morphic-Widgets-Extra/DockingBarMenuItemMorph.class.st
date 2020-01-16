"
A DockingBarMenuItemMorph is a specialized version of its superclass for DockingBar behavior.

"
Class {
	#name : #DockingBarMenuItemMorph,
	#superclass : #MenuItemMorph,
	#category : #'Morphic-Widgets-Extra'
}

{ #category : #events }
DockingBarMenuItemMorph >> activateOwnerMenu: evt [
	^ self owner ifNil: [ false ] ifNotNil: [ :o | o activate: evt. true]
]

{ #category : #selecting }
DockingBarMenuItemMorph >> adjacentTo [
	| verticalMargin |
	verticalMargin := 4 + owner borderWidth.
	owner isFloating
		ifTrue: [^ {self bounds bottomLeft + (5 @ verticalMargin)}].
	owner isAdheringToTop
		ifTrue: [^ {self bounds bottomLeft + (5 @ verticalMargin)}].
	owner isAdheringToLeft
		ifTrue: [^ {self bounds topRight + (5 @ verticalMargin)}].
	owner isAdheringToBottom
		ifTrue: [^ {self bounds topLeft + (5 @ verticalMargin)}].
	owner isAdheringToRight
		ifTrue: [^ {self bounds topLeft + (5 @ verticalMargin negate)}].
	^ {self bounds bottomLeft + (3 @ verticalMargin)}
]

{ #category : #events }
DockingBarMenuItemMorph >> deselectTimeOut: evt [
	"Deselect timout. Now really deselect"

	owner selectedItem == self
		ifTrue: [ 
			evt hand releaseMouseFocus: owner.
			owner selectItem: nil event: evt ]
]

{ #category : #events }
DockingBarMenuItemMorph >> mouseDown: evt [
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	(evt shiftPressed and: [ self isEditable ])
		ifTrue: [^ super mouseDown: evt].  "enable label editing"
	isSelected
		ifTrue: [ owner selectItem: nil event: evt ]
		ifFalse: [ owner activate: evt.	"Redirect to menu for valid transitions"
			owner selectItem: self event: evt ]
]

{ #category : #events }
DockingBarMenuItemMorph >> mouseEnter: evt [
	"The mouse entered the receiver"

	owner ifNotNil: [owner stayUp ifFalse: [self mouseEnterDragging: evt]].
	(owner selectedItem notNil
				and: [owner selectedItem ~~ self])
			ifTrue: [owner selectItem: self event: evt.].


]

{ #category : #events }
DockingBarMenuItemMorph >> mouseUp: evt [
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	evt hand mouseFocus == owner ifFalse: [^self].
	
	self contentString ifNotNil:[
		self contents: self contentString withMarkers: true inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	owner rootMenu selectItem: nil event: evt.
	self invokeWithEvent: evt.		

]

{ #category : #'submorphs-accessing' }
DockingBarMenuItemMorph >> noteNewOwner: aMorph [ 
	"I have just been added as a submorph of aMorph"
	super noteNewOwner: aMorph.

	self updateLayoutInDockingBar
]

{ #category : #'change reporting' }
DockingBarMenuItemMorph >> ownerChanged [
	"The receiver's owner, some kind of a pasteup, has changed its 
	layout."
	super ownerChanged.
	self updateLayoutInDockingBar
]

{ #category : #'submorphs-accessing' }
DockingBarMenuItemMorph >> roundedCorners [
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

{ #category : #private }
DockingBarMenuItemMorph >> subMenuMarker [
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

{ #category : #private }
DockingBarMenuItemMorph >> updateLayoutInDockingBar [

	owner isVertical
		ifTrue: [
			self hResizing: #spaceFill.
			self vResizing: #shrinkWrap]
		ifFalse: [
			self hResizing: #shrinkWrap.
			self vResizing: #spaceFill].
	self extent: self minWidth @ self minHeight
]

{ #category : #rounding }
DockingBarMenuItemMorph >> wantsRoundedCorners [
	^self theme currentSettings preferRoundCorner
		
]
