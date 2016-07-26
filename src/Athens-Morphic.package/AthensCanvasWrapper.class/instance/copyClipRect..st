formcanvas copying
copyClipRect: aRectangle
"dont know why this exists, all calls with a different cliprect should go through"
"clipBy:during: and I don't know if this properly works with athens canvases because"
"we can not handle to different clipstates for the same canvas."
	^ self copyOrigin: origin clipRect: aRectangle
