primitives
primCreate: cairoSurface

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #( 
		AthensCairoCanvas cairo_create (AthensCairoSurface cairoSurface) )