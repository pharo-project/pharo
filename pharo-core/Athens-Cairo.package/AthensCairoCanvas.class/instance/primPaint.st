private
primPaint
	"A drawing operator that paints the current source everywhere within the current clip region."
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(void cairo_paint (self))
