primitives
primSetExtend: extend

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall:#(
		void cairo_pattern_set_extend ( self , ulong extend) )
		