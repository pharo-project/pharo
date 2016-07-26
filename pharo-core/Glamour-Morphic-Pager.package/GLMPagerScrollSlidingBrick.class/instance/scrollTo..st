slider-scrolling
scrollTo: aFloat "Float - percentage/100"
	"scrolls to a specified float number which must be in interval [0.0, 1.0]"
	self moveLeft: (owner width - self width) * aFloat.