instance creation queries
dosEpoch
	"Answer a DateAndTime representing the DOS epoch (1 January 1980, midnight UTC)"

	^ self basicNew
		ticks: #(2444240 0 0) offset: Duration zero;
		yourself.