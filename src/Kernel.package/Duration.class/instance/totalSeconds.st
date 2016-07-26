accessing
totalSeconds
	"Return the total number of seconds in me.
	If I have zero nano seconds, the result is equivalent to #seconds. 
	If I have nanoseconds, the result will contain fractional seconds."
	
	"1500 milliSeconds totalSeconds"

	^ nanos = 0
		ifTrue: [ seconds ]
		ifFalse: [ self asNanoSeconds / 1e9 ]