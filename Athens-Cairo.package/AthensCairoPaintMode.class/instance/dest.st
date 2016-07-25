modes
dest
	"Set 'dest' drawing mode.
	
	Drawing under this mode will ignore the source, as if nothing drawn"
		
	^ self setOperator: CAIRO_OPERATOR_DEST