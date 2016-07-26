modes
clear
	"Set 'clear' drawing mode.
	
	Drawing under this mode will clear the surface under the shape boundaries"
	
	^ self setOperator: CAIRO_OPERATOR_CLEAR