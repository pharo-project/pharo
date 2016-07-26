accessing
wrapScrolling: aBoolean
	"Set whether tab scrolling wraps around at each end."

	wrapScrolling := aBoolean.
	self
		changed: #leftButtonEnabled;
		changed: #rightButtonEnabled