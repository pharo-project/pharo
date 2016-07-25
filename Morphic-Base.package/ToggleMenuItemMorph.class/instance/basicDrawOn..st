drawing-private
basicDrawOn: aCanvas 
	"Draw on the canvas. Taken from MenuItemMorph for minor refactoring."
	
	| stringColor stringBounds|
	stringColor := self stringColorToUse.
	stringBounds := self stringBoundsToUse.
	(self isSelected and: [self isEnabled]) ifTrue: [
		aCanvas
			fillRectangle: self bounds
			fillStyle: self selectionFillStyle
			borderStyle: self selectionBorderStyle].
	self hasIcon ifTrue: [ |iconForm|
		iconForm := self icon.
		self drawIcon: iconForm on: aCanvas in: stringBounds.
		stringBounds := stringBounds left: stringBounds left + iconForm width + 2].
	self hasMarker ifTrue: [
		stringBounds := stringBounds left: stringBounds left + self submorphBounds width + 8].
	stringBounds := stringBounds top: stringBounds top + stringBounds bottom - self fontToUse height // 2.
	stringBounds := stringBounds bottom: stringBounds top + self fontToUse height.
	self drawText: contents on: aCanvas in: stringBounds.
	self hasSubMenu ifTrue: [ |subMenuMarker| 
		subMenuMarker := self subMenuMarker deepCopy mapColor: Color black to: stringColor.
		self drawSubMenuMarker: subMenuMarker on: aCanvas in: stringBounds]