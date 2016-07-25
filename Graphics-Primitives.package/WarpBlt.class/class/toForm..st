initialization
toForm: destinationForm
	"Default cell size is 1 (no pixel smoothing)"
	^ (super toForm: destinationForm) cellSize: 1