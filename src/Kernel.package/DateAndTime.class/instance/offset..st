accessing
offset: anOffset
	"Answer a <DateAndTime> equivalent to the receiver but with its local time 
	being offset from UTC by offset.
	Unlike #translateTo: this will NOT change the absolute in UTC "

	^ self class basicNew 
		ticks: self ticks offset: anOffset asDuration;
		yourself
		