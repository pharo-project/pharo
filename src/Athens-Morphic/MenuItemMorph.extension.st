Extension { #name : #MenuItemMorph }

{ #category : #'*Athens-Morphic' }
MenuItemMorph >> drawBackgroundOnAthensCanvas: anAthensCanvas [
	(isSelected and: [ isEnabled ])
		ifFalse: [ ^ self ].
	anAthensCanvas setPaint: self selectionFillStyle.
	anAthensCanvas drawShape: (0 @ 0 extent: self extent)
]

{ #category : #'*Athens-Morphic' }
MenuItemMorph >> drawIconAndLabelOnAthensCanvas: anAthensCanvas [
	anAthensCanvas pathTransform
		restoreAfter: [
			self drawIconOnAthensCanvas: anAthensCanvas.
			self drawLabelOnAthensCanvas: anAthensCanvas ]
]

{ #category : #'*Athens-Morphic' }
MenuItemMorph >> drawIconOnAthensCanvas: anAthensCanvas [
	self hasIcon
		ifFalse: [ ^ self ].
	anAthensCanvas setPaint: self iconForm.
	anAthensCanvas drawShape: (0 @ 0 extent: self iconForm extent).
	anAthensCanvas pathTransform translateX: self iconForm width + 2 Y: 0
]

{ #category : #'*Athens-Morphic' }
MenuItemMorph >> drawLabelOnAthensCanvas: anAthensCanvas [
	anAthensCanvas pathTransform translateX: 0 Y: self fontToUse getPreciseAscent.
			anAthensCanvas
				setPaint: self color;
				setFont: self fontToUse;
				drawString: contents
]

{ #category : #'*Athens-Morphic' }
MenuItemMorph >> drawOnAthensCanvas: anAthensCanvas [
	| bnd |
	bnd := self bounds topLeft + self layoutInset.
	anAthensCanvas pathTransform
		restoreAfter: [
			anAthensCanvas pathTransform translateBy: bnd.
			self drawBackgroundOnAthensCanvas: anAthensCanvas.
			self drawIconAndLabelOnAthensCanvas: anAthensCanvas.
			self drawSubMenuMarkerOnAthensCanvas: anAthensCanvas ]
]

{ #category : #'*Athens-Morphic' }
MenuItemMorph >> drawSubMenuMarkerOnAthensCanvas: anAthensCanvas [
	self hasSubMenu
		ifFalse: [ ^ self ].
	anAthensCanvas pathTransform
		restoreAfter: [
			anAthensCanvas pathTransform translateX: self extent x - self subMenuMarker width Y: 0.
			anAthensCanvas pathTransform translateX: 0 Y: (self subMenuMarker height //2).
			anAthensCanvas setPaint: self subMenuMarker.
			anAthensCanvas drawShape: (0 @ 0 extent: self subMenuMarker extent) ]
]
