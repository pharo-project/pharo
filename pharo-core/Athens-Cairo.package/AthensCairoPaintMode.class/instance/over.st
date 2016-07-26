modes
over
	"Set 'over' drawing mode. This is default paint mode.
	
	Drawing under this mode will blend source with destination color using source alpha component"
			
	^ self setOperator: CAIRO_OPERATOR_OVER