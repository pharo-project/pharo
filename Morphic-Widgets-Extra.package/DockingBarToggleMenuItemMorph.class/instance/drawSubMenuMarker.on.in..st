drawing-private
drawSubMenuMarker: aForm on: aCanvas in: aRectangle
	"Draw the submenu marker on the canvas within the given bounds."
	
	|markerRect| 
	markerRect := aRectangle topRight + (aForm width negated @ (aRectangle height - aForm height // 2)) extent: aForm extent.
	markerRect translateBy: -4@1.
	self drawIcon: aForm on: aCanvas in: markerRect