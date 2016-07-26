accessing
offset: anOffset
	"Answer a <Timespan> equivalent to the receiver but with its local time 
	being offset from UTC by offset.
	Unlike #translateTo: this will NOT change the absolute in UTC "

	^ self class 
		starting: (self start offset: anOffset) 
		duration: self duration