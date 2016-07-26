instance creation queries
unixEpoch
	"Answer a DateAndTime representing the Unix epoch (1 January 1970, midnight UTC)"
	^ self basicNew
		ticks: #(2440588 0 0) offset: Duration zero;
		yourself.