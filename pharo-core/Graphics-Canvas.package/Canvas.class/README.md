A canvas is a two-dimensional medium on which morphs are drawn in a device-independent manner. Canvases keep track of the origin and clipping rectangle, as well as the underlying drawing medium (such as a window, pixmap, or postscript script).

Subclasses must implement (at least) the following methods:
	* Drawing:
		#fillOval:color:borderWidth:borderColor:
		#frameAndFillRectangle:fillColor:borderWidth:borderColor:
		#drawPolygon:color:borderWidth:borderColor:
		#image:at:sourceRect:rule:
		#stencil:at:sourceRect:rule:
		#line:to:width:color:
		#paragraph:bounds:color:
		#text:bounds:font:color:
	* Support
		#clipBy:during:
		#translateBy:during:
		#translateBy:clippingTo:during:
		#transformBy:clippingTo:during:
