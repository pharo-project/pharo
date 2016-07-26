accessing
wholeNanoseconds
	"Answer the number of whole nanoseconds in me after whole seconds, milli & microseconds are removed"

	"1002003004 nanoSeconds wholeNanoseconds"

	^ nanos rem: 1e3