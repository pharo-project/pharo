modes
source
	"Set 'source' drawing mode.
	
	Drawing under this mode will replace the content with incoming source under the shape boundaries"
	
	^ self setOperator: CAIRO_OPERATOR_SOURCE