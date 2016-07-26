accessing
wholeMicroseconds
	"Answer the number of whole microseconds in me after whole seconds and milliseconds are removed"
	
	"1002003004 nanoSeconds wholeMicroseconds"

	^ (nanos rem: 1e6) quo: 1e3