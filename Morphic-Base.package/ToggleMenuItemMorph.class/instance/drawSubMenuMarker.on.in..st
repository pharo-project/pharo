drawing-private
drawSubMenuMarker: aForm on: aCanvas in: aRectangle
	"Draw the submenu marker on the canvas within the given bounds."
	
	|markerRect| 
	markerRect := aRectangle topRight + (aForm width negated @ (aRectangle height - aForm height // 2)) extent: aForm extent.
	self drawIcon: aForm on: aCanvas in: markerRect