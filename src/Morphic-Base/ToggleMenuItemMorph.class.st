"
A menu item that supports on/off state (using standard icons), enablement and display of ""accelerator"" key text.
"
Class {
	#name : 'ToggleMenuItemMorph',
	#superclass : 'MenuItemMorph',
	#instVars : [
		'getStateSelector',
		'enablementSelector'
	],
	#category : 'Morphic-Base-Menus',
	#package : 'Morphic-Base',
	#tag : 'Menus'
}

{ #category : 'selecting' }
ToggleMenuItemMorph >> adjacentTo [

	^{self bounds topRight + (5 @ 0). self bounds topLeft + (2@0)}
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> adoptPaneColor: paneColor [
	"Pass on to submenu too."

	super adoptPaneColor: paneColor.
	self hasSubMenu ifTrue: [self subMenu adoptPaneColor: paneColor]
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> argumentsBlock: aBlock [

	self arguments: aBlock value
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> balloonText: aText [

	self assureExtension balloonText: aText
]

{ #category : 'private - drawing' }
ToggleMenuItemMorph >> basicDrawOn: aCanvas [
	"Draw on the canvas. Taken from MenuItemMorph for minor refactoring."

	| stringColor stringBounds maxItemsIconWidth |
	stringColor := self stringColorToUse.
	stringBounds := self stringBoundsToUse.
	(self isSelected and: [self isEnabled]) ifTrue: [
		aCanvas
			fillRectangle: self bounds
			fillStyle: self selectionFillStyle
			borderStyle: self selectionBorderStyle].
	self hasIcon ifTrue: [
		self drawIcon: self iconFormSet on: aCanvas in: stringBounds ].
	maxItemsIconWidth := self owner maxItemsIconWidth.
	maxItemsIconWidth > 0 ifTrue: [
		stringBounds := stringBounds left: stringBounds left + maxItemsIconWidth + (2*self displayScaleFactor) ].
	self hasMarker ifTrue: [
		stringBounds := stringBounds left: stringBounds left + self submorphBounds width + (8 * self displayScaleFactor)].
	stringBounds := stringBounds top: stringBounds top + stringBounds bottom - self fontToUse height // 2.
	stringBounds := stringBounds bottom: stringBounds top + self fontToUse height.
	self drawText: contents on: aCanvas in: stringBounds.
	self hasSubMenu ifTrue: [ |subMenuMarkerFormSet colorMappedFormSet|
		subMenuMarkerFormSet := self subMenuMarkerFormSet.
		colorMappedFormSet := FormSet extent: subMenuMarkerFormSet extent depth: subMenuMarkerFormSet depth
			forms: (subMenuMarkerFormSet forms collect: [ :subMenuMarkerForm |
				subMenuMarkerForm deepCopy mapColor: Color black to: stringColor ]).
		self drawSubMenuMarker: colorMappedFormSet on: aCanvas in: stringBounds]
]

{ #category : 'private - drawing' }
ToggleMenuItemMorph >> drawIcon: aFormSet on: aCanvas in: aRectangle [
	"Draw the icon on the canvas within the given bounds."

	| toggledIconFormSet |
	toggledIconFormSet := self isEnabled
		ifTrue: [ aFormSet ]
		ifFalse: [
			| baseIconForm toggledIconForm |
			baseIconForm := aFormSet asFormAtScale: aCanvas scale.
			toggledIconForm := Form extent: baseIconForm extent depth: 32.
			toggledIconForm fillColor: (Color white alpha: 0.003922).
			(toggledIconForm getCanvas asAlphaBlendingCanvas: 0.5)
				drawImage: baseIconForm
				at: 0@0.
			FormSet extent: aFormSet extent depth: 32 forms: { toggledIconForm } ].
	aCanvas
		translucentFormSet: toggledIconFormSet
		at: aRectangle topLeft + (0@(aRectangle height - toggledIconFormSet height // 2))
]

{ #category : 'private - drawing' }
ToggleMenuItemMorph >> drawKeyTextOn: aCanvas [
	"Draw the key text on the canvas."

	self keyText ifNil: [^self].
	self itemShortcut drawOn: aCanvas
]

{ #category : 'drawing' }
ToggleMenuItemMorph >> drawOn: aCanvas [
	"Need to check isEnabled here."

	self
		isEnabled;
		basicDrawOn: aCanvas;
		drawKeyTextOn: aCanvas
]

{ #category : 'private - drawing' }
ToggleMenuItemMorph >> drawSubMenuMarker: aFormSet on: aCanvas in: aRectangle [
	"Draw the submenu marker on the canvas within the given bounds."

	|markerRect|
	markerRect := aRectangle topRight + ((aFormSet width * self displayScaleFactor) negated @ (aRectangle height - (aFormSet height * self displayScaleFactor) // 2)) extent: aFormSet extent * self displayScaleFactor.
	self drawIcon: (FormSet extent: aFormSet extent scaledByDisplayScaleFactor truncated depth: aFormSet depth forms: aFormSet forms) on: aCanvas in: markerRect
]

{ #category : 'private - drawing' }
ToggleMenuItemMorph >> drawText: aStringOrText on: aCanvas in: aRectangle [
	^ self
		drawText: aStringOrText
		on: aCanvas
		in: aRectangle
		color: self stringColorToUse
]

{ #category : 'private - drawing' }
ToggleMenuItemMorph >> drawText: aStringOrText on: aCanvas in: aRectangle color: stringColor [
	"Draw the text on the canvas within the given bounds."

	(self enabled not and: [self theme disabledItemStyle = #inset])
		ifTrue: [
				aCanvas
					drawString: aStringOrText
					in: aRectangle
					font: self fontToUse
					color: stringColor muchLighter lighter;
					drawString: aStringOrText
					in: (aRectangle translateBy: -1)
					font: self fontToUse
					color: stringColor]
		ifFalse: [aCanvas
					drawString: aStringOrText
					in: aRectangle
					font: self fontToUse
					color: stringColor ]
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> enablementSelector [
	"Answer the value of enablementSelector"

	^ enablementSelector
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> enablementSelector: anObject [
	"Set the value of enablementSelector"

	enablementSelector := anObject
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> getStateSelector [
	"Answer the value of getStateSelector"

	^ getStateSelector
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> getStateSelector: anObject [
	"Set the value of getStateSelector"

	getStateSelector := anObject
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> iconFormSet [
	"Answer the receiver's icon form set. Handle legacy case
	of wording-based mechanism."

	|state|
	self getStateSelector ifNil: [^super iconFormSet].
	state := (MessageSend receiver: self target selector: self getStateSelector)
		valueWithEnoughArguments: self arguments .
	self flag: #pharoFixMe.
	(state isKindOf: Association)
		ifTrue: [^ state key ifTrue: [FormSet form: self onImage] ifFalse: [FormSet form: self offImage]]
		ifFalse: [
	(state = true or: [state isString and: [(state beginsWith: '<yes>') or: [state beginsWith: '<on>']]])
		ifTrue: [^FormSet form: self onImage].
	(state = false or: [state isString and: [(state beginsWith: '<no>') or: [state beginsWith: '<off>']]])
		ifTrue: [^FormSet form: self offImage]].
	^super iconFormSet
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> isEnabled [
	"Answer whether the item is enabled."

	|state|
	self enablementSelector ifNil: [^super isEnabled].
	state := self enablementSelector isSymbol
		ifTrue: [self target perform: self enablementSelector]
		ifFalse: [self enablementSelector value].
	self isEnabled: state.
	^state
]

{ #category : 'testing' }
ToggleMenuItemMorph >> isEnabled: aBoolean [

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled := aBoolean.
	self changed
]

{ #category : 'private - drawing' }
ToggleMenuItemMorph >> itemShortcut [

	^ ToggleMenuItemShortcut
		owner: self
		keyText: self keyText
]

{ #category : 'private' }
ToggleMenuItemMorph >> keyTextExpanded [
	^ ToggleMenuItemShortcut normalize: self keyText
]

{ #category : 'layout' }
ToggleMenuItemMorph >> minWidth [
	"Plus the key text if any."

	|w|
	w := super minWidth.
	self keyText ifNotNil: [w := w + (self fontToUse widthOfString: self keyTextExpanded) + 12].
	^w
]

{ #category : 'events' }
ToggleMenuItemMorph >> mouseEnter: evt [
	"The mouse entered the receiver.
	Handle the case when in an EmbeddedMenuMorph."

	super mouseEnter: evt.
	(owner isNotNil and: [owner isKindOf: EmbeddedMenuMorph]) ifTrue:[
		owner selectedItem ~~ self
			ifTrue: [owner selectItem: self event: evt]]
]

{ #category : 'events' }
ToggleMenuItemMorph >> mouseLeave: evt [
	"The mouse left the receiver.
	Handle the case when in an EmbeddedMenuMorph."

	super mouseLeave: evt.
	(owner isNotNil and: [owner isKindOf: EmbeddedMenuMorph]) ifTrue:[
		owner selectedItem == self
			ifTrue: [owner selectItem: nil event: evt]]
]

{ #category : 'private' }
ToggleMenuItemMorph >> offImage [
	"Return the form to be used for indicating an '<off>' marker."

	|m form|
	m := CheckboxButtonMorph new
		privateOwner: self owner;
		adoptPaneColor: self paneColor;
		selected: false.
	form := Form extent: m extent depth: 32.
	form fillColor: (Color white alpha: 0.003922).
	form getCanvas fullDrawMorph: m.
	^form
]

{ #category : 'private' }
ToggleMenuItemMorph >> onImage [
	"Return the form to be used for indicating an '<on>' marker."

	|m form|
	m := CheckboxButtonMorph new
		privateOwner: self owner;
		adoptPaneColor: self paneColor;
		selected: true.
	form := Form extent: m extent depth: 32.
	form fillColor: (Color white alpha: 0.003922).
	form getCanvas fullDrawMorph: m.
	^form
]

{ #category : 'selecting' }
ToggleMenuItemMorph >> select: evt [
	"Don't if not enabled."

	self isEnabled ifFalse: [^self].
	^super select: evt
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> selectionBorderStyle [
	"Answer the border style to use with the receiver is the selected element."

	^ self theme menuItemSelectedBorderStyleFor: self
]

{ #category : 'private' }
ToggleMenuItemMorph >> selectionFillStyle [
	"Answer the fill style to use with the receiver is the selected element."

	^ self theme menuItemSelectedFillStyleFor: self
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> stringBoundsToUse [
	"Answer the bounds to use when drawing the item text."

	^ self bounds
]

{ #category : 'accessing' }
ToggleMenuItemMorph >> stringColorToUse [
	"Answer the state dependent color to use for drawing text."

	^self isEnabled
		ifTrue: [
			self isSelected
				ifTrue: [ self theme menuItemSelectedTextColorFor: self ]
				ifFalse: [ self theme menuItemNormalTextColorFor: self ] ]
		ifFalse: [
				self theme menuItemDisabledTextColorFor: self ]
]
