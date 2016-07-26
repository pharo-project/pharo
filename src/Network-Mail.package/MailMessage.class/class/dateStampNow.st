utilities
dateStampNow
	"Return the current date and time formatted per RFC5322  e.g.  'Thu, 18 Feb 1999 20:38:51 -0500'"

	^	DateAndTime now asEmailString.