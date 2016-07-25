drawing
drawOn: aCanvas 
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